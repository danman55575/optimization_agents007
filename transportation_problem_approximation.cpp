#include <iostream>
#include <vector>
#include <iomanip>
#include <string>
#include <limits.h>
#include <utility>
#include <queue>
#include <algorithm>

using namespace std;

class Table {
private:
    int n, m;
    vector<vector<int>> table;
    vector<bool> valid_rows;
    vector<bool> valid_columns;
public:
    Table(vector<vector<int>>& costs, vector<int>& supply, vector<int>& demand) {

        n = supply.size();
        m = demand.size();

        table = vector<vector<int>>(n+1, vector<int>(m+1));
        valid_rows.assign(n, 1);
        valid_columns.assign(m, 1);

        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                table[i][j] = costs[i][j];
            }
        }
        for (int i = 0; i < n; i++) {
            table[i][m] = supply[i];
        }
        for (int i = 0; i < m; i++) {
            table[n][i] = demand[i];
        }
    }

    bool is_valid_row(int row) const {
        return valid_rows[row];
    }

    bool is_valid_column(int col) const {
        return valid_columns[col];
    }

    void put_cell_capacity(int row, int col, int val) {
        table[row][m] -= val;
        table[n][col] -= val;
        if (table[row][m] == 0) {
            valid_rows[row] = false;
        }
        if (table[n][col] == 0) {
            valid_columns[col] = false;
        }
    }

    int get_row_capacity(int row) const {
        return table[row][m];
    }

    int get_col_capacity(int col) const {
        return table[n][col];
    }

    int get_min_index_in_row(int row) {
        if (is_valid_row(row)) {
            int min_value = INT_MAX, col_index = -1;
            for (int col = 0; col < m; col++) {
                if (is_valid_column(col) && table[row][col] < min_value) {
                    min_value = table[row][col];
                    col_index = col;
                }
            }
            return col_index;
        }
        return -1;
    }

    int get_min_index_in_col(int col) {
        if (is_valid_column(col)) {
            int min_value = INT_MAX, row_index = -1;
            for (int row = 0; row < n; row++) {
                if (is_valid_row(row) && table[row][col] < min_value) {
                    min_value = table[row][col];
                    row_index = row;
                }
            }
            return row_index;
        }
        return -1;
    }

    int get_row_diff(int row) {
        if (is_valid_row(row)) {
            vector<int> values;
            for (int col = 0; col < m; col++) {
                if (is_valid_column(col)) {
                    values.push_back(table[row][col]);
                }
            }
            sort(values.begin(), values.end());
            int k = values.size();
            if (k == 0) return -1;
            if (k == 1) return 0;
            return values[k-1]-values[k-2];
        } else {
            return -1;
        }
    }

    int get_column_diff(int col) {
        if (is_valid_column(col)) {
            vector<int> values;
            for (int row = 0; row < n; row++) {
                if (is_valid_row(row)) {
                    values.push_back(table[row][col]);
                }
            }
            sort(values.begin(), values.end());
            int k = values.size();
            if (k == 0) return -1;
            if (k == 1) return 1;
            return values[k-1]-values[k-2];
        } else {
            return -1;
        }
    }

    int get_column_largest_cost(int col) {
        int max_val = -1e9;
        if (is_valid_column(col)) {
            for (int row = 0; row < n; row++) {
                if (is_valid_row(row)) {
                    max_val = max(max_val, table[row][col]);
                }
            }
        }
        return max_val;
    }

    int get_row_largest_cost(int row) {
        int max_val = -1e9;
        if (is_valid_row(row)) {
            for (int col = 0; col < m; col++) {
                if (is_valid_column(col)) {
                    max_val = max(max_val, table[row][col]);
                }
            }
        }
        return max_val;
    }

    int get_row_number() const {
        return n;
    }

    int get_column_number() const {
        return m;
    }

    int get_cost(int row, int col) const {
        return table[row][col];
    }

    void show() const {
        cout << "Initial table:\n";
        cout << setw(7) << "-";
        for (int i = 0; i < m; i++) {
            cout << setw(7) << i << ") ";
        }
        cout << setw(7) << "Supply\n";
        for (int i = 0; i < n; i++) {
            cout << setw(7) << i << ") ";
            for (int j = 0; j < m+1; j++) {
                cout << setw(7) << table[i][j] << " ";
            }
            cout << "\n";
        }
        cout << setw(7) << "   Demand";
        for (int i = 0; i < m; i++) {
            cout << setw(7) << table[n][i] << " ";
        }
        cout << setw(7) << "-\n";
    }
};

struct tp_solution {
private:
    int n, m;
    vector<vector<int>> table;
public:
    void init(int row_number, int column_number) {
        n = row_number;
        m = column_number;
        table = vector<vector<int>>(n, vector<int>(m, 0));
    }
    void set(int row, int col, int val) {
        table[row][col] = val;
    }
    friend ostream& operator<<(ostream& out, const tp_solution& sol) {
        for (int row = 0; row < sol.n; row++) {
            for (int col = 0; col < sol.m; col++) {
                out << setw(7) << sol.table[row][col] << " ";
            }
            out << "\n";
        }
        return out;
    }
};

struct position {
    int row;
    int col;

    bool operator==(const position& other) {
        return (this->row == other.row && this->col == other.col);
    }
};

tp_solution north_west_algorithm(Table init_table) {
    int n = init_table.get_row_number(), m = init_table.get_column_number();
    
    tp_solution sol;
    sol.init(n, m);

    position pos = {0, 0};
    position end_pos = {n-1, m-1};

    while (pos.row < n && pos.col < m) {
        int row_cap = init_table.get_row_capacity(pos.row);
        int col_cap = init_table.get_col_capacity(pos.col);
        int cell_cap = min(row_cap, col_cap);

        init_table.put_cell_capacity(pos.row, pos.col, cell_cap);
        sol.set(pos.row, pos.col, cell_cap);

        if (pos.col < m && init_table.is_valid_row(pos.row))
            pos.col++;
        else
            pos.row++;
    }

    return sol;
}

tp_solution vogel_algorithm(Table init_table) {
    int n = init_table.get_row_number(), m = init_table.get_column_number();
    
    tp_solution sol;
    sol.init(n, m);

    while (true) {
        bool choice_is_row = false;
        int index = -1, largest_diff = -1;
        for (int row = 0; row < n; row++) {
            int current_diff = init_table.get_row_diff(row);
            if (current_diff > largest_diff) {
                largest_diff = current_diff;
                choice_is_row = true;
                index = row;
            }
        }
        for (int col = 0; col < m; col++) {
            int current_diff = init_table.get_column_diff(col);
            if (current_diff > largest_diff) {
                largest_diff = current_diff;
                choice_is_row = false;
                index = col;
            }
        }

        if (largest_diff == -1) break;

        int row, col;
        
        if (choice_is_row) {
            row = index;
            col = init_table.get_min_index_in_row(row);
        } else {
            col = index;
            row = init_table.get_min_index_in_col(col);
        }

        int row_cap = init_table.get_row_capacity(row);
        int col_cap = init_table.get_col_capacity(col);
        int cell_cap = min(row_cap, col_cap);
        init_table.put_cell_capacity(row, col, cell_cap);
        sol.set(row, col, cell_cap);
    }

    return sol;

}

tp_solution russel_algorithm(Table init_table) {
    int n = init_table.get_row_number(), m = init_table.get_column_number();
    
    tp_solution sol;
    sol.init(n, m);

    while (true) {
        vector<int> rows_max(n), columns_max(m);
        for (int row = 0; row < n; row++)
            rows_max[row] = init_table.get_row_largest_cost(row);
        for (int col = 0; col < m; col++)
            columns_max[col] = init_table.get_column_largest_cost(col);
        
        position best_cell = {-1, -1};
        int best_val = 1e9;
        for (int row = 0; row < n; row++) {
            for (int col = 0; col < m; col++) {
                int value = init_table.get_cost(row, col) - rows_max[row] - columns_max[col];
                if (value < best_val) {
                    best_val = value;
                    best_cell = {row, col};
                }
            }
        }

        if (best_val > 0)
            break;
        
        int row_cap = init_table.get_row_capacity(best_cell.row);
        int col_cap = init_table.get_col_capacity(best_cell.col);
        int cell_cap = min(row_cap, col_cap);
        init_table.put_cell_capacity(best_cell.row, best_cell.col, cell_cap);
        sol.set(best_cell.row, best_cell.col, cell_cap);
    }

    return sol;

}

int main() {
    int n, m;

    cout << "Input number of sources: ";
    cin >> n;
    cout << "Input number of destinations: ";
    cin >> m;

    vector<vector<int>> costs(n, vector<int>(m));
    vector<int> supply(n);
    vector<int> demand(m);

    cout << "Input sources weights (" << n << "):\n";
    for (int i = 0; i < n; i++) {
        cin >> supply[i];
    }

    cout << "Input destination weights (" << m << "):\n";
    for (int i = 0; i < m; i++) {
        cin >> demand[i];
    }

    cout << "Input cost matrix (" << n << " rows x " << m << " columns):\n";
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++) {
            cin >> costs[i][j];
        }
    }

    Table init_table(costs, supply, demand);
    init_table.show();

    tp_solution north_west_sol = north_west_algorithm(init_table);
    tp_solution vogel_sol = vogel_algorithm(init_table);
    tp_solution russel_sol = russel_algorithm(init_table);
    cout << "North-West solution:\n" << north_west_sol << "\n";
    cout << "Vogel's solution:\n" << vogel_sol << "\n";
    cout << "Russel's solution:\n" << russel_sol << "\n";
    return 0;
}
