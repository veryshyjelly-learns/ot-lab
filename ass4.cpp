/*
 * Name: Ayush Biswas
 * Roll: 21MA10015
 * Assignment: 4
 */
#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace std;

enum cmp
{
    le,
    ge,
    eq
};

int lastnonzeroindex(vector<int> coeffs, int n);
cmp flip(cmp a);

int main()
{
    int n, m; // n is the number of variables and m is the number of equations
    cout << "Enter the number of equations: ";
    cin >> m;
    cout << "Enter the number of variables: ";
    cin >> n;

    string ptype;
    cout << "Enter problem type (max or min): ";
    cin >> ptype;
    vector<int> z(n, 0);
    cout << "Enter the costs: ";
    for (int i = 0; i < n; i++)
    {
        cin >> z[i];
    }

    if (ptype == "min")
    {
        for (int i = 0; i < n; i++)
        {
            z[i] = -z[i];
        }
    }

    vector<vector<int>> coefficients(m, vector<int>(n, 0));
    vector<int> constants(m, 0);
    vector<cmp> ords(m, cmp::eq);

    cout << "Enter equations in format: " << endl;
    cout << "ai1 ai2 ... ain <le or ge or eq> bi" << endl;

    // take input of the equations
    for (int i = 0; i < m; i++)
    {
        for (int j = 0; j < n; j++)
        {
            cin >> coefficients[i][j];
        }
        string ord;
        cin >> ord;
        if (ord == "le")
        {
            ords[i] = cmp::le;
        }
        else if (ord == "ge")
        {
            ords[i] = cmp::ge;
        }
        else if (ord == "eq")
        {
            ords[i] = cmp::eq;
        }
        else
        {
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
    for (int i = 0; i < c; i++)
    {
        int x;
        cin >> x;
        constrained[x - 1] = true;
    }

    // make all bi's positive
    for (int i = 0; i < m; i++)
    {
        if (constants[i] < 0)
        {
            ords[i] = flip(ords[i]);
            for (int j = 0; j < n; j++)
            {
                coefficients[i][j] = -coefficients[i][j];
            }
            constants[i] = -constants[i];
        }
    }

    vector<int> variabletype(n, 0);

    // add slack or surplus variables
    for (int i = 0; i < m; i++)
    {
        if (ords[i] == cmp::le)
        {
            coefficients[i].push_back(1);
            for (int j = 0; j < m; j++)
            {
                if (j == i)
                    continue;
                coefficients[j].push_back(0);
            }
            ords[i] = cmp::eq;
            constrained.push_back(true);
            variabletype.push_back(1);
            n++;
        }
        else if (ords[i] == cmp::ge)
        {
            coefficients[i].push_back(-1);
            for (int j = 0; j < m; j++)
            {
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
        if (coefficients[i][n - 1] != 1)
        {
            coefficients[i].push_back(1);
            variabletype.push_back(3);
            constrained.push_back(true);
            for (int j = 0; j < m; j++)
            {
                if (j == i)
                    continue;
                coefficients[j].push_back(0);
            }
            n++;
        }
    }

    // now make all the variables constrained
    for (int i = 0; i < n; i++)
    {
        if (!constrained[i])
        {
            for (int j = 0; j < m; j++)
            {
                coefficients[j].insert(coefficients[j].begin() + i + 1, -coefficients[j][i]);
            }
            z.insert(z.begin() + i + 1, -z[i]);
            constrained.insert(constrained.begin() + i + 1, true);
            variabletype.insert(variabletype.begin() + i + 1, 0);
            n++;
        }
    }

    z.resize(n, 0);
    vector<vector<int>> X(n, vector<int>(m, 0));
    for (int i = 0; i < n; i++)
    {
        for (int j = 0; j < m; j++)
        {
            X[i][j] = coefficients[j][i];
        }
    }
    // getting the basis
    vector<int> basis(m, 0);
    for (int i = 0; i < m; i++)
    {
        basis[i] = lastnonzeroindex(coefficients[i], n + 1) + 1;
    }

    vector<int> b(constants);
    vector<int> Xb(constants);
    vector<int> Nb;
    for (int i = 0, j = 0; i < n; i++)
    {
        if (basis[j] == i)
        {
            j++;
            continue;
        }
        Nb.push_back(i);
    }
    vector<int> C(z);
    vector<int> Cb(m);
    for (int i = 0; i < m; i++)
        Cb[i] = C[i];
}

int lastnonzeroindex(vector<int> coeffs, int n)
{
    for (int i = n - 1; i >= 0; i--)
    {
        if (coeffs[i] != 0)
        {
            return i;
        }
    }
    return 0;
}

cmp flip(cmp a)
{
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