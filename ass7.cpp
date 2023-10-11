/*
 * Name: Ayush Biswas
 * Roll: 21MA10015
 * Assignment: 7
 */
#include <iostream>
#include <string>
#include <vector>
#include <cmath>

using namespace std;

enum cmp {
    le, ge, eq
};

int lastoneindex(vector<double> coeffs, int n);
cmp flip(cmp a);

int main()
{
    int n, m; // n is the number of variables and m is the number of equations
    cout << "Enter the number of equations: "; cin >> m;
    cout << "Enter the number of variables: "; cin >> n;

    string ptype;
    cout << "Enter problem type (max or min): "; cin >> ptype;

    vector<double> z(n, 0);
    cout << "Enter the costs: ";
    for (int i = 0; i < n; i++) cin >> z[i];
    

    if (ptype == "min") {
        for (int i = 0; i < n; i++) {
            z[i] = -z[i];
        }
    }

    vector<vector<double>> coefficients(m, vector<double>(n, 0));
    vector<double> constants(m, 0);
    vector<cmp> ords(m, cmp::eq);

    cout << "Enter equations in format: " << endl;
    cout << "ai1 ai2 ... ain <le or ge or eq> bi" << endl;

    bool unbuonded_solution = false;

    // take input of the equations
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) cin >> coefficients[i][j];
        string ord; cin >> ord;
        if (ord == "le") {
            ords[i] = cmp::le;
        }
        else if (ord == "ge") {
            ords[i] = cmp::ge;
        }
        else if (ord == "eq") {
            ords[i] = cmp::eq;
        }
        else {
            cout << "error: incorrect format";
            exit(1);
        }
        cin >> constants[i];
    }

    // getting the variable constraints
    cout << "Now enter the index (if variable is xi enter i) of the constraint variables" << endl;
    int c;
    cout << "Enter the count: ";
    cin >> c;

    cout << "Enter the indexes: ";
    vector<bool> constrained(n, false);
    for (int i = 0; i < c; i++) {
        int x;
        cin >> x;
        constrained[x - 1] = true;
    }

    // make all bi's positive
    for (int i = 0; i < m; i++) {
        if (constants[i] < 0) {
            ords[i] = flip(ords[i]);
            for (int j = 0; j < n; j++) {
                coefficients[i][j] = -coefficients[i][j];
            }
            constants[i] = -constants[i];
        }
    }

    vector<int> variabletype(n, 0);

    // add slack or surplus variables
    for (int i = 0; i < m; i++) {
        if (ords[i] == cmp::le) {
            coefficients[i].push_back(1);
            for (int j = 0; j < m; j++) {
                if (j == i)
                    continue;
                coefficients[j].push_back(0);
            }
            ords[i] = cmp::eq;
            constrained.push_back(true);
            variabletype.push_back(1);
            n++;
        }
        else if (ords[i] == cmp::ge) {
            coefficients[i].push_back(-1);
            for (int j = 0; j < m; j++) {
                if (j == i)
                    continue;
                coefficients[j].push_back(0);
            }
            ords[i] = cmp::eq;
            constrained.push_back(true);
            variabletype.push_back(2);
            n++;
        }

        // now add artificial variables to make basis possible
        if (coefficients[i][n - 1] != 1) {
            coefficients[i].push_back(1);
            variabletype.push_back(3);
            constrained.push_back(true);
            for (int j = 0; j < m; j++) {
                if (j == i)
                    continue;
                coefficients[j].push_back(0);
            }
            n++;
        }
    }

    // now make all the variables constrained
    for (int i = 0; i < n; i++) {
        if (!constrained[i]) {
            for (int j = 0; j < m; j++) {
                coefficients[j].insert(coefficients[j].begin() + i + 1, -coefficients[j][i]);
            }
            z.insert(z.begin() + i + 1, -z[i]);
            constrained.insert(constrained.begin() + i + 1, true);
            variabletype.insert(variabletype.begin() + i + 1, 0);
            n++;
        }
    }

    z.resize(n, 0);

    // Big-M method
    for (int i = 0; i < n; i++) {
        if (variabletype[i] == 3) {
            z[i] = -100000.0;
        }
    }

    vector<vector<double>> X(n, vector<double>(m, 0));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            X[i][j] = coefficients[j][i];
        }
    }
    // getting the basis
    vector<int> basis(m, 0);
    for (int i = 0; i < m; i++) {
        basis[i] = lastoneindex(coefficients[i], n);
    }

    vector<double> b(constants);
    vector<double> C(z);
    vector<double> Cb(m);
    vector<double> delta(n, 0);
    vector<double> x;
    for (int i = 0, j = 0; i < n; i++) {
        if (basis[j] == i) {
            x.push_back(constants[j]);
            j++;
            continue;
        }
        x.push_back(0);
    }

// *********************************

    cout << "C:    ";
    for (int i = 0; i < n; i++) {
        cout << C[i] << " ";
    }
    cout << endl;
    cout << "XB CB ";
    for (int i = 0; i < n; i++) {
        cout << "X" << i+1 << " ";
    }
    cout << "b" << endl;
    for (int i = 0; i < m; i++) {
        cout << "x" << basis[i] << " ";
        cout << (int) Cb[i] << " ";
        for (int j = 0; j < n; j++) {
            cout << X[j][i] << " ";
        }
        cout << b[i] << endl;
    }
    cout << "delta ";
    for (int i = 0; i < n; i++) {
        double zj = 0;
        for (int j = 0; j < m; j++) {
            zj += Cb[j] * X[i][j];
        }
        cout << zj - C[i] << " ";
    }
// **********************************


    int altercount = 0;

    double zvalue = 0.0;
    for (int xyz = 0; xyz < 300 ;xyz++) {
        vector<double> Nb;
        for (int i = 0, j = 0; i < n; i++) {
            if (basis[j] == i) {
                j++;
                continue;
            }
            Nb.push_back(i);
        }

        cout << '\n';
        cout << "Iteration " << xyz+1 << endl;

        cout << "BFS: ";
        for (size_t i = 0; i < basis.size(); i++) {
            cout << "x" << basis[i]+1 << "=" << x[basis[i]] << " ";
        }  
        cout << endl;

        cout << "Basic variables: ";
        for (size_t i = 0; i < basis.size(); i++) {
            cout << "x" << basis[i]+1 << " "; 
        }
        cout << endl;

        cout << "Non-Basic variables: ";
        for (size_t i = 0; i < Nb.size(); i++) {
            cout << "x" << Nb[i]+1 << " ";
        }
        cout << endl;

        zvalue = 0.0;
        for (int i = 0; i < n; i++) {
            zvalue += C[i] * x[i];
        }
        cout <<  "Objective value = " << zvalue << endl;

        cout << "delta ";
        for (int i = 0; i < n; i++) {
            double zj = 0;
            for (int j = 0; j < m; j++) {
                zj += Cb[j] * X[i][j];
            }
            delta[i] = zj - C[i];
            cout << delta[i] << " ";
        }
        cout << endl;

        bool allpositive = true;
        for (int i = 0, j = 0; i < n; i++) {
            if (delta[i] <= 0 && basis[j] != i) allpositive = false;
            if (basis[j] == i) j++;
        }
        if (allpositive) {
            break;
        };

        bool allpositivepos = true;
        for (int i = 0, j = 0; i < n; i++) {
            if (delta[i] < 0 && basis[j] != i) allpositivepos = false;
            if (basis[j] == i) j++;
        }
        if (allpositivepos && altercount > 2*m) {
            cout << "Infinite Number of solutions" << endl;
            break;
        } else if (allpositivepos) {
            altercount++;
        };

        int k = 0;
        for (k = 0; k < n; k++) {
            bool f = false;
            for (int i = 0; i < m; i++) {
                if (basis[i] == k) {
                    f = true;
                }
            }
            if (!f) break;
        }

        double deltamin = delta[k];    
        for (int i = 0; i < n; i++) {
            bool f = false;
            for (int j = 0; j < m; j++) {
                if (basis[j] == i) {
                    f = true;
                }
            }
            if (f) continue;
            if (delta[i] < deltamin) {
                k = i;
                deltamin = delta[i];
            }
        }

        int s = 0;
        double minratio = 100000000;
        bool minratioduplicate = false;
        vector<int> ss;
        ss.push_back(0);
        bool mrfound = false;

        for (int i = 0; i < m; i++) {
            if (X[k][i] <= 0) continue;
            if ((x[basis[i]]/X[k][i] < minratio) && (x[basis[i]]/X[k][i] >= 0)) {
                minratio = x[basis[i]]/X[k][i];
                minratioduplicate = false;
                ss.clear();
                ss.push_back(i);
                s = i;
                mrfound = true;
            } else if ((x[basis[i]]/X[k][i] == minratio) && (x[basis[i]]/X[k][i] >= 0)) {
                minratioduplicate = true;
                ss.push_back(i);
            }
        }

        if (mrfound == false) {
            unbuonded_solution = true;
            cout << endl << "Unbounded Solution" << endl;
            break;
        }


        for (int j = m; j < n; j++) {
            if (!minratioduplicate) break;
            cout << "Degenerate tie breaking" << endl;

            double minratioxyz = 1000000.0;
            minratioduplicate = false;

            for (int i = 0; i < ss.size(); i++) {
                int index = ss[i];
                if ((X[j][index]/X[k][index] < minratioxyz) && (X[j][index]/X[k][index] >= 0)) {
                    minratioxyz = X[j][index]/X[k][index];
                    minratioduplicate = false;
                    s = index;
                    cout << "s = " << index << endl;
                } else if (X[j][index]/X[k][index] == minratioxyz) {
                    minratioduplicate = true;
                }
            }
        }

        cout << "Min ratio = " << minratio << endl;

        cout << "Pivot element = (" << k+1 << "," << s+1 << ") = " << X[k][s] << endl;

        for (int j = 0; j < m; j++) {
            if (j == s) {
                x[basis[j]] = x[basis[j]]/X[k][s];
            } else {
                x[basis[j]] = x[basis[j]] - minratio*X[k][j];
            } 
        }

        for (int j = 0; j < m; j++) {
            for (int i = 0; i < n; i++) {
                if (i == k) {
                    ;
                } else if (j == s) {
                    X[i][j] = X[i][j] / X[k][s];
                } else {
                    X[i][j] = X[i][j] - (X[k][j]*X[i][s])/X[k][s];
                }
            }
        }

        for (int j = 0; j < m; j++) {
            if (j == s) X[k][j] = 1;
            else X[k][j] = 0;
        }

        for (int j = 0 ; j < m; j++) {
            for (int i = 0; i < n; i++) {
                cout << X[i][j] << " ";
            }
            cout << endl;
        }


        cout << "Incoming variable: x" << k+1 << endl;
        cout << "Outgoing variable: x" << basis[s]+1 << endl; 
        Cb[s] = C[k];
        x[k] = x[basis[s]];
        if (basis[s] == k) break;
        x[basis[s]] = 0;
        basis[s] = k;
        cout << endl;
    }

    // if (!unbuonded_solution) {
        cout << endl;
        cout << "Optimal solution: " << endl;
        for (size_t i = 0; i < basis.size(); i++) {
            cout << "x" << basis[i]+1 << "=" << x[basis[i]] << " ";
        }  
        cout << endl;
        cout << "Optimal value: " << zvalue << endl;
    // }


}

int lastoneindex(vector<double> coeffs, int n)
{
    int ans = 0;
    for (int i = 0; i < n; i++)
    {
        if (abs(coeffs[i] - 1) < 1e-3)
        {
            ans = i;
        }
    }
    return ans;
}

cmp flip(cmp a) {
    if (a == cmp::le)
    {
        return cmp::ge;
    }
    else if (a == cmp::ge)
    {
        return cmp::le;
    }
    return cmp::eq;
}