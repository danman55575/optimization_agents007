#include <iostream>
#include <algorithm>
#include <vector>
#include <cmath>
#include <utility>
#include <string>
#include <stdexcept>

using namespace std;

struct solution {
    bool solvable;
    vector<double> optimal_decision;
    double objective_function;
};

double eps;

/**
 * @brief Simplex matrix represents a matrix for iterative finding the optimal solution 
 * for given objective function and constraints. The constraints must be represented as follows:
 * 1) a * x + ... + b * z <= k
 * ...
 * n) v * x + ... + c * y <= p 
 * 
 * It is assumed that all the variables must be greater than zero!
 * 
 * The Simplex Matrix solve the problem of maximization by default!
 */
class SimplexMatrix {
private:
    int n_, m_;
    int number_of_initial_vars;
    double eps;
    vector<vector<double>> table_;

    void check_input_correctness(vector<double>& objFunc, vector<vector<double>>& constrFunc, vector<double>& b) {
        if (objFunc.size() == 0) {
            throw invalid_argument("Objective function does not depend on variables!");
        }
        if (constrFunc.size() == 0) {
            throw invalid_argument("Constraint matrix must have at least one constraint!");
        }
        if (constrFunc[0].size() != objFunc.size()) {
            throw invalid_argument("Objective function and constrFunc[0] must have the same size!");
        }
        if (b.size() != constrFunc.size()) {
            throw invalid_argument("Vector of values and constrFunc must have the same size!");
        }
    }
public:
    SimplexMatrix() {}

    void build(vector<double>& objFunc, vector<vector<double>>& constrFunc, vector<double>& b) {
        check_input_correctness(objFunc, constrFunc, b);

        this->number_of_initial_vars = objFunc.size();
        int n = constrFunc.size(), m = constrFunc[0].size();
        vector<int> equations_with_slack_vars(n,-1);
        int num_eq_with_sl_vars = 0;

        for (int col = 0; col < m; col++) {
            int number_of_occurs = 0, equations_num = -1;
            for (int row = 0; row < n; row++) {
                number_of_occurs += (constrFunc[row][col] != 0);
                equations_num = row;
            }
            if (number_of_occurs == 0) {
                throw invalid_argument("There exists variable with no occurrences in constraint function!");
            }
            if (number_of_occurs == 1) {
                equations_with_slack_vars[equations_num] = col;
                num_eq_with_sl_vars++;
            }
        }

        int actual_n = n + 2;
        int actual_m = m + 2 + (n - num_eq_with_sl_vars);
        this->table_ = vector<vector<double>> (actual_n, vector<double> (actual_m, 0));

        // fill table without slack variables and index column
        for (int col = 1; col < int(objFunc.size())+1; col++) {
            table_[0][col] = col-1;
            table_[1][col] = -objFunc[col-1];
        }
        for (int row = 2; row < actual_n; row++) {
            for (int col = 1; col < m+1; col++) {
                table_[row][col] = constrFunc[row-2][col-1];
            }
            table_[row].back() = b[row-2];
        }

        // fill slack variables' coefficients and index column
        int col_slack = 1 + objFunc.size();
        for (int row = 2; row < actual_n; row++) {
            if (equations_with_slack_vars[row-2] == -1) {
                table_[row][col_slack] = 1;
                table_[row][0] = col_slack-1;
                table_[0][col_slack] = col_slack-1;
                col_slack++;
            } else {
                table_[row][0] = equations_with_slack_vars[row-2];
            }
        }

        n_ = actual_n;
        m_ = actual_m;
    }

    void do_iteration() {
        double previous_z = table_[1].back();
        // find variable with minimum coefficient
        long double min_value = 1e9;
        int replaced_col = -1;
        for (int col = 1; col < m_ - 1; col++) {
            if (table_[1][col] < min_value && table_[1][col] < -eps) {
                min_value = table_[1][col];
                replaced_col = col;
            }
        }
        if (replaced_col == -1) return;
        // find the minimum positive pivot
        long double pivot = 1e9;
        int replaced_row = -1;
        for (int row = 2; row < n_; row++) {
            if (abs(table_[row][replaced_col]) <= 0) continue;
            long double cur_pivot = table_[row].back() / table_[row][replaced_col];
            if (cur_pivot < 0) continue;
            if (cur_pivot <= eps) cur_pivot = 0;
            if (cur_pivot < pivot) {
                pivot = cur_pivot;
                replaced_row = row;
            }
        }
        if (replaced_row == -1) {
            throw runtime_error("Solution does not exist!");
        }
        // change the vars' indexes
        table_[replaced_row][0] = table_[0][replaced_col];
        // Gauss-Jordan elimination
        long double initial_value = table_[replaced_row][replaced_col];
        for (int col = 1; col < m_; col++) {
            table_[replaced_row][col] /= initial_value;
            if (abs(table_[replaced_row][col]) <= eps) {
                table_[replaced_row][col] = 0;
            }
        }

        for (int row = 1; row < n_; row++) {
            if (row == replaced_row) continue;
            long double coef = -table_[row][replaced_col];
            if (coef == 0) continue;
            for (int col = 1; col < m_; col++) {
                table_[row][col] += table_[replaced_row][col] * coef;
            }
        }
        if (table_[1].back() == previous_z) return;
        do_iteration();
    }

    void show_problem(const vector<double>& objFunc, const vector<vector<double>>& constrFunc, const vector<double>& b) const {
        int n = constrFunc.size(), m = objFunc.size();
        cout << "Problem\n  Maximize z = ";
        for (int col = 0; col < m; col++) {
            if (col != 0) {
                if (objFunc[col] >= 0) cout << "+ ";
                else cout << "- ";
            }
            cout << abs(objFunc[col]) << " * x" << col+1 << " ";
        }
        cout << "\n  Subject to constraints:\n";
        for (int row = 0; row < n; row++) {
            cout << "    ";
            for (int col = 0; col < m; col++) {
                if (col != 0) {
                    if (constrFunc[row][col] >= 0) cout << "+ ";
                    else cout << "- ";
                }
                cout << abs(constrFunc[row][col]) << " * x" << col+1 << " ";
            }
            cout << "<= " << b[row] << "\n";
        }
    }

    void show() const {
        for (int row = 0; row < n_; row++) {
            for (int col = 0; col < m_; col++) {
                cout << table_[row][col] << " ";
            }
            cout << "\n";
        }
    }

    solution find_solution(vector<double>& objFunc, vector<vector<double>>& constrFunc, vector<double>& b, double eps) {
        this->eps = eps;
        show_problem(objFunc, constrFunc, b);
        try {
            build(objFunc, constrFunc, b);
            do_iteration();
        } catch (...) {
            return solution{false, {}, 0.0};
        }
        vector<double> decision_vars(number_of_initial_vars, 0);
        for (int row = 2; row < n_; row++) {
            if (table_[row][0] < number_of_initial_vars) {
                decision_vars[table_[row][0]] = table_[row].back();
            }
        }
        return solution{true, decision_vars, table_[1].back()};
    }
};


int main() {
    /*
    Input must be of the following format:
        1) first line contains two integers n & m, where n - number of variables, m - number of constraints
        2) the second line consists of n numbers: coefficients before each variable of objective function
        3) next m lines contains the constraints: each line contains the coefficients before each variable of current contraint
        4) the last line contains m numbers: solution for each constraint
    */
    freopen("input.txt", "r", stdin);
    int n, m;
    cin >> n >> m;
    vector<double> objFunc(n);
    vector<vector<double>> constrs(m, vector<double>(n));
    vector<double> b(m);
    for (int i = 0; i < n; i++) cin >> objFunc[i];
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) cin >> constrs[i][j];
    }
    for (int i = 0; i < m; i++) cin >> b[i];
    cin >> eps;
    SimplexMatrix sm;
    solution ans = sm.find_solution(objFunc, constrs, b, eps);
    cout << "Answer\n";
    if (!ans.solvable) {
        cout << "  The method is not applicable!\n";
    } else {
        cout << "  z = " << ans.objective_function << "\n";
        for (int i = 0; i < n; i++) {
            cout << "  x" << i+1 << " = " << ans.optimal_decision[i] << "\n";
        }
    }
    return 0;
}
