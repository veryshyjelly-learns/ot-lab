#include <bits/stdc++.h>

using namespace std;

enum EquationType {
    le, ge, eq
};

enum ProblemType {
    max, min
};

enum VariableType {
    normal, slack, surplus, artificial 
};

enum VariableState {
    basic, non_basic
};

enum DegenracyState {
    positive_delta, negative_delta, zero_delta, degenarate, unbounded
};

class Variable {
public:
    string name;
    float weight;
    VariableType type;
    VariableState state;
    Variable(string naam, float wt, VariableType tp) {
        name = naam;
        weight = wt;
        type = tp;
        state = VariableState::non_basic;
    }

    void flip_type() {
        if (type == VariableType::slack) type = VariableType::surplus;
        else if (type == VariableType::surplus) type = VariableType::slack;
    }
};

typedef pair<Variable*, float> Term;

class Equation {
public:
    vector<Term> terms;
    float constant;

    Equation() {
        terms.resize(0);
    }
};

class InEquation : public Equation {
public:
    EquationType type;

    InEquation() {
        type = EquationType::eq;
    }

    void flip_type() {
        if (type == EquationType::le) type = EquationType::ge;
        else if (type == EquationType::ge) type = EquationType::le;  
    }

    void abs_constant() {
        if (constant < 0) {
            flip_type();
            for (auto &t : terms) {
                t.first->flip_type();
                t.second *= -1;
            }
            constant *= -1;
        }
    }
};


class Problem {
public: 
    ProblemType type;
    vector<Variable*> variables;
    vector<InEquation> equations;
    Problem(string tp) {
        variables.resize(0); equations.resize(0);
        if (tp == "max") {
            type = ProblemType::max;
        } else if (tp == "min") {
            type = ProblemType::min;
        } else {
            cout << "ERROR: Invalid Problem Type";
            exit(1);
        }
    }

    void make_primal() {
        if (type == ProblemType::max) {
            for (auto &eq : equations) if (eq.type == EquationType::ge) {
                for (auto &t: eq.terms) {
                    t.second *= -1;
                }
                eq.constant *= -1;
                eq.flip_type();
            }
        } else if (type == ProblemType::min) {
            for (auto &eq : equations) if (eq.type == EquationType::le) {
                for (auto &t: eq.terms) {
                    t.second *= -1;
                }
                eq.constant *= -1;
                eq.flip_type();
            }
        }
    }

    Problem* dual() {
        make_primal();
        string ptype;
        EquationType etype;
        if (type == ProblemType::max) {
            ptype = "min";
            etype = EquationType::ge;
        } else {
            ptype = "max";
            etype = EquationType::le;
        }

        Problem* new_problem = new Problem(ptype);
        int n = equations.size();
        int m = variables.size();

        for (int i = 0; i < n; i++) {
            string naam = "w" + to_string(i+1);
            new_problem->variables.push_back(new Variable(naam, equations[i].constant, VariableType::normal));
        }

        for (int i = 0; i < m; i++) {
           InEquation new_eq = InEquation();
            for (int j = 0; j < n; j++) {
                double a = equations[j].terms[i].second;
                new_eq.terms.push_back({new_problem->variables[j], a});
            }
            new_eq.type = etype;
            new_eq.constant = variables[i]->weight;
            new_problem->equations.push_back(new_eq); 
        }

        vector<bool> constrained(n, false);
        for (int i = 0; i < n; i++) {
            constrained[i] = equations[i].type != EquationType::eq;
        }
        // Making all variables constrained
        for (int i = 0; i < n; i++) {
            if (!constrained[i]) {
                Variable* var = new_problem->variables[i];
                new_problem->variables.insert(new_problem->variables.begin() + i + 1, 
                    new Variable(var->name + "'", -var->weight, var->type));

                for (int j = 0; j < m; j++) {
                    new_problem->equations[j].terms.insert(new_problem->equations[j].terms.begin() + i + 1, 
                        {new_problem->variables[i+1], -1*new_problem->equations[j].terms[i].second});
                }
                constrained.insert(constrained.begin() + i + 1, true);
                n++;
            }
        }

        return new_problem;
    }

    void show() {
        if (type == ProblemType::max) cout << "max ";
        else cout << "min ";

        cout << "Z = ";
        for (auto &var : variables) cout << var->weight << var->name << "+"; cout << "\b" << " " << "\n";
        for (auto &eq : equations) {
            for (auto &t : eq.terms) cout << t.second << t.first->name << "+"; cout << "\b";
            if (eq.type == le) {
                cout << " <= " << eq.constant << endl;
            } else if (eq.type == ge) {
                cout << " >= " << eq.constant << endl;
            } else {
                cout << " = " << eq.constant << endl;
            } 
        }
    }
};

class StandardProblem {
public:
    ProblemType type;
    DegenracyState state;
    vector<Variable*> variables;
    vector<Equation> equations;
    vector<Variable*> bfs;
    vector<pair<Variable*, float>> delta;
    vector<float*> ratios;
    pair<int, int>* pivot_point;
    Variable* last_left;

    StandardProblem() {
        variables.resize(0); equations.resize(0); bfs.resize(0); delta.resize(0);
    }

    void initialize_delta() {
        delta.resize(0);
        for (auto var : variables) delta.push_back({var, 0});
    }

    void initialize_minratio() {
        ratios.resize(equations.size());
        for (int i = 0; i < equations.size(); i++) ratios[i] = NULL;
    }

    void compute_delta() {
        // delta.resize(variables.size());
        for (int i = 0; i < variables.size(); i++) {
            delta[i].second = 0;
            for (int j = 0; j < equations.size(); j++) {
                delta[i].second += equations[j].terms[i].second * bfs[j]->weight;
            }
            delta[i].second -= variables[i]->weight;
        }
        update_degenracy_state();
    }

    void update_degenracy_state() {
        float delta_min = 1;
        for (auto d : delta) {
            if (d.first->state != VariableState::basic) {
                // cout << "Comparing = " << d.first->name << " = " << d.second << endl;
                if (delta_min > d.second) delta_min = d.second;
            }
        }
        // cout << "Delta Min = " << delta_min << endl;
        if (delta_min < 0) {
            state = DegenracyState::negative_delta;
        } else if (delta_min == 0) {
            state = DegenracyState::zero_delta;
        } else {
            state = DegenracyState::positive_delta;
        }
    }

    void compute_pivot() {
        pivot_point = NULL;
        compute_delta();
        if (state == DegenracyState::positive_delta) return;
        int k = 0, delta_min = 1;
        for (int i = 0; i < variables.size(); i++) {
            if (delta[i].first->state != VariableState::basic && delta[i].second < delta_min) {
                delta_min = delta[i].second;
                k = i;
            }
        }

        bool duplicate = false;
        int s = -1;
        float min_ratio = 1e5;
        compute_ratios(k, -1);
        for (int i = 0; i < equations.size(); i++) {
            if (ratios[i] != NULL && ((*ratios[i]) <= min_ratio)) {
                min_ratio = *ratios[i];
                s = i; 
            }
        }
        if (s == -1) {
            state = DegenracyState::unbounded;
            return;
        }
        if (state == DegenracyState::zero_delta) state = DegenracyState::degenarate;
        pivot_point = new pair<int, int>({k, s});
    }

    void compute_ratios(int k, int b) {
        if (b == -1) {
            for (int i = 0; i < equations.size(); i++) {
                ratios[i] = NULL;
                if (equations[i].terms[k].second > 0) {
                    // float* ratio = (float*) malloc(sizeof(float));
                    ratios[i] = new float(0);
                    *ratios[i] = equations[i].constant / equations[i].terms[k].second;
                    // cout << "constant = " << equations[k].constant << " ";
                    // cout << "coeff = " << equations[i].terms[k].second << " ";
                    // cout << "ratios[" << i << "]=" << *ratios[i] << endl;
                    // ratios[i] = ratio;
                }
            } 
        } else {
            for (int i = 0; i < equations.size(); i++) {
                ratios[i] = NULL;
                if (equations[i].terms[k].second > 0) {
                    // float* ratio = (float*) malloc(sizeof(float));
                    ratios[i] = new float(0);
                    *ratios[i] = equations[i].terms[k].second / equations[i].terms[b].second;
                    // ratios[i] = ratio;
                }             
            } 
        }
    }

    void solve_two_phase() {
        int arti_count = 0;
        for (auto &var : variables) if (var->type == VariableType::artificial) arti_count++;
        if (arti_count == 0) return solve_big_m();
        map<Variable*, double> weights;
        // vector<double> weights;
        for (auto &var : variables) {
            weights[var] = var->weight;
            // weights.push_back(var->weight); 
            var->weight = 0;
            if (var->type == VariableType::artificial) var->weight = -1;
        }

        cout << "------- Phase 1 ---------" << endl;

        while (true) {
            show();

            bool arti_present = false;
            for (auto &v : bfs) if (v->type == VariableType::artificial) arti_present = true;
            if (!arti_present) break;

            if (state == DegenracyState::degenarate || pivot_point == NULL) break;
            float pivot = equations[pivot_point->second].terms[pivot_point->first].second; 
            for (auto &t : equations[pivot_point->second].terms) t.second /= pivot;
            equations[pivot_point->second].constant /= pivot;
            for (int i = 0; i < equations.size(); i++) {
                if (i == pivot_point->second) continue;
                for (int j = 0; j < variables.size(); j++) {
                    if (j == pivot_point->first) continue;
                    equations[i].terms[j].second -= equations[i].terms[pivot_point->first].second *
                             equations[pivot_point->second].terms[j].second;
                }
                equations[i].constant -= equations[i].terms[pivot_point->first].second * equations[pivot_point->second].constant;
                equations[i].terms[pivot_point->first].second -= equations[i].terms[pivot_point->first].second *
                             equations[pivot_point->second].terms[pivot_point->first].second;
            }
            if (variables[pivot_point->first] == last_left) {
                state = DegenracyState::degenarate;
                break;
            }
            bfs[pivot_point->second]->state = VariableState::non_basic;
            last_left = bfs[pivot_point->second];
            bfs[pivot_point->second] = variables[pivot_point->first];
            bfs[pivot_point->second]->state = VariableState::basic;
        }

        bool arti_present = false;
        for (auto &v : bfs) if (v->type == VariableType::artificial) arti_present = true;
        if (arti_present) {
            cout << "Unbounded solution" << endl;
            return;
        }    
        for (int i = 0; i < variables.size();) {
            if (variables[i]->type == VariableType::artificial) {
                variables.erase(variables.begin() + i);
                for (auto &eq : equations) eq.terms.erase(eq.terms.begin() + i);
            } else {
                i++;
            }
        }

        for (auto &var : variables) var->weight = weights[var];
        cout << "------- Phase 2 ---------" << endl;
        solve_big_m();
    }

    void solve_big_m() {
        for (auto &var : variables) {
            if (var->type == VariableType::artificial) {
                var->weight = -999;
            }
        }
        
        while (true) {
            show();
            if (state == DegenracyState::degenarate || pivot_point == NULL) break;
            float pivot = equations[pivot_point->second].terms[pivot_point->first].second; 
            for (auto &t : equations[pivot_point->second].terms) t.second /= pivot;
            equations[pivot_point->second].constant /= pivot;
            for (int i = 0; i < equations.size(); i++) {
                if (i == pivot_point->second) continue;
                for (int j = 0; j < variables.size(); j++) {
                    if (j == pivot_point->first) continue;
                    equations[i].terms[j].second -= equations[i].terms[pivot_point->first].second *
                             equations[pivot_point->second].terms[j].second;
                }
                equations[i].constant -= equations[i].terms[pivot_point->first].second * equations[pivot_point->second].constant;
                equations[i].terms[pivot_point->first].second -= equations[i].terms[pivot_point->first].second *
                             equations[pivot_point->second].terms[pivot_point->first].second;
            }
            if (variables[pivot_point->first] == last_left) {
                state = DegenracyState::degenarate;
                break;
            }
            bfs[pivot_point->second]->state = VariableState::non_basic;
            last_left = bfs[pivot_point->second];
            bfs[pivot_point->second] = variables[pivot_point->first];
            bfs[pivot_point->second]->state = VariableState::basic;
        }
        if (state == DegenracyState::degenarate) {
            cout << "Degenarate Solution" << endl;
        } else if (state == DegenracyState::unbounded) {
            cout << "Unbounded Solution" << endl;
        } else if (state == DegenracyState::positive_delta) {
            cout << "Solution: ";
            float z = 0;
            for (int i = 0; i < bfs.size(); i++) z += bfs[i]->weight * equations[i].constant;
            z = type == ProblemType::max ? z : -1 * z;
            cout << "z=" << z << ", ";
            for (int i = 0; i < bfs.size(); i++) {
                if (bfs[i]->type == VariableType::normal) {
                    cout << bfs[i]->name << "=" << equations[i].constant << ",";
                }
            }
            for (auto &var : variables) {
                if (var->type == VariableType::normal && var->state == VariableState::non_basic) {
                    cout << var->name << "=0,";
                }
            }
            cout << endl;
        } else if (pivot_point == NULL) {
            cout << "Pivot somehow null" << endl;
        }
    }

    void show() {
        std::ios oldState(nullptr);
        oldState.copyfmt(std::cout);

        compute_pivot();
        int n = variables.size();
        int m = equations.size();

        cout << "\n";

        cout << setw(21) << "cj->";
        for (auto var: variables) cout << setw(7) <<  var->weight; cout << endl;

        cout << setw(7) << "B/V";
        cout << setw(7) << "CB";

        cout << setw(7) << "XB";
        for (auto var: variables) cout << setw(7) <<  var->name;
        
        cout << setw(8) << "XB/Xk" << endl; 

        cout << setprecision(4);
        for (int i = 0; i < m; i++) {
            cout << setw(7) << bfs[i]->name;
            cout << setw(7) << bfs[i]->weight;
            cout << setw(7) << equations[i].constant;
            for (auto tr: equations[i].terms) cout << setw(7) << tr.second; 
            if (ratios[i] != NULL) {
                cout << setw(8) << (*ratios[i]) << endl;
            } else {
                cout << setw(8) << "-" << endl;
            }
        }

        float z = 0;
        for (int i = 0; i < bfs.size(); i++) z += bfs[i]->weight * equations[i].constant;

        cout << setw(14) << "z = " << setw(7) << left << z;
        cout << right;
        for (auto d : delta) cout << setw(7) << d.second; 
        cout << setw(8) << " <-Delta" << endl;

        if (pivot_point != NULL) {
            cout << "leaving: " << bfs[pivot_point->second]->name << ", ";
            cout << "entering: " << variables[pivot_point->first]->name << endl;
        }

        std::cout.copyfmt(oldState);
    }
};

Problem read_problem();
StandardProblem* convert_problem(Problem problem);

int main() {
    // StandardProblem* problem = convert_problem(read_problem());
    // problem->solve_two_phase();
    Problem problem = read_problem();
    Problem* dual_problem = problem.dual();
    problem.show();
    cout << endl;
    dual_problem->show();

    convert_problem(problem)->solve_two_phase();
    convert_problem(*dual_problem)->solve_big_m();
    // problem->solve_big_m();
    // problem->show();
}

StandardProblem* convert_problem(Problem problem) {
    int n = problem.variables.size();
    int m = problem.equations.size();

    StandardProblem* standard_problem = new StandardProblem();
    standard_problem->type = problem.type;

    if (problem.type == ProblemType::min) {
        for (int i = 0; i < n; i++) {
            problem.variables[i]->weight *= -1;
        }
    }

    for (int i = 0; i < n; i++) {
        standard_problem->variables.push_back(problem.variables[i]);
    }

    for (int i = 0; i < m; i++) {
        Equation new_eq = Equation();
        problem.equations[i].abs_constant();

        for (int j = 0; j < problem.variables.size(); j++) {
            new_eq.terms.push_back(problem.equations[i].terms[j]);
        }

        for (int k = problem.variables.size(); k < standard_problem->variables.size(); k++) {
            new_eq.terms.push_back({standard_problem->variables[k], 0});
        }

        string naam = "s" + to_string(i+1);
        bool arti_add = false;

        switch (problem.equations[i].type) {
        case EquationType::le:
        // Add Slack Variable
            standard_problem->variables.push_back(new Variable(naam, 0, VariableType::slack));
            new_eq.terms.push_back({standard_problem->variables[n], 1});
            n++; break;
        case EquationType::ge:
        // Add Surplus Variable
            standard_problem->variables.push_back(new Variable(naam, 0, VariableType::surplus));  
            new_eq.terms.push_back({standard_problem->variables[n], -1});
            arti_add = true;
            n++; break;
        default: 
            arti_add = true;
            break;
        }

        string arti_naam = "a" + to_string(i+1);
        if (arti_add) {
        // Add Artificial Variable
            standard_problem->variables.push_back(new Variable(arti_naam, 0, VariableType::artificial));  
            new_eq.terms.push_back({standard_problem->variables[n], 1});
            n++;
        }

        new_eq.constant = problem.equations[i].constant;
        standard_problem->equations.push_back(new_eq);
        standard_problem->variables[n-1]->state = VariableState::basic;
        standard_problem->bfs.push_back(standard_problem->variables[n-1]);
    }

    for (int i = 0; i < m; i++) {
    // Update terms for all the equations
        for (int k = standard_problem->equations[i].terms.size(); k < n; k++) {
            standard_problem->equations[i].
                terms.push_back({standard_problem->variables[k], 0});
        }
    }

    // Initialize min ratio and delta
    standard_problem->initialize_delta();
    standard_problem->initialize_minratio();

    return standard_problem; 
}

Problem read_problem() {
    string ptyp; cout << "(min or max)? "; cin >> ptyp;
    Problem problem = Problem(ptyp);
    int n; cout << "Number of variables? "; cin >> n;
    int m; cout << "Number of equations? "; cin >> m;

    cout << "Enter C: ";
    for (int i = 0; i < n; i++) {
        string naam = "x" + to_string(i+1);
        float w; cin >> w;
        problem.variables.push_back(new Variable(naam, w, VariableType::normal));
    }

    cout << "Enter equations in format: ai1 ai2 ... ain <le or ge or eq> bi\n";
    for (int i = 0; i < m; i++) {
        InEquation new_eq = InEquation();
        for (int j = 0; j < n; j++) {
            float a; cin >> a;
            new_eq.terms.push_back({problem.variables[j], a});
        }
        string tp; cin >> tp;
        if (tp == "le") {
            new_eq.type = EquationType::le;
        } else if (tp == "ge") {
            new_eq.type = EquationType::ge;
        } else if (tp == "eq") {
            new_eq.type = EquationType::eq;
        } else {
            cout << "ERROR: invalid equation type";
            exit(1);
        }
        cin >> new_eq.constant;
        problem.equations.push_back(new_eq);
    }

    int c; cout << "Constrained variable count: "; cin >> c;
    cout << "Enter the index (if variable is xi enter i): ";
    vector<bool> constrained(n, false);
    for (int i = 0; i < c; i++) {
        int x; cin >> x; constrained[x - 1] = true;
    }

    // Making all variables constrained
    for (int i = 0; i < n; i++) {
        if (!constrained[i]) {
            Variable* var = problem.variables[i];
            problem.variables.insert(problem.variables.begin() + i + 1, 
                new Variable(var->name + "'", -var->weight, var->type));

            for (int j = 0; j < m; j++) {
                problem.equations[j].terms.insert(problem.equations[j].terms.begin() + i + 1, 
                    {problem.variables[i+1], -1*problem.equations[j].terms[i].second});
            }
            constrained.insert(constrained.begin() + i + 1, true);
            n++;
        }
    }

    return problem;
}
