#include <iostream>
#include <stdio.h>
#include <sstream>
#include <fstream>
using namespace std;
void destroy( float ** elements,
              unsigned int rows )
{
    for( unsigned int i = 0; i < rows; ++i ) {
        delete [] elements[ i ];
    }
    delete [] elements;
}

float** create_matrix(unsigned int columns,
                      unsigned int rows)
{
    float** matrix;
    matrix = new float*[rows];
    for (unsigned int i = 0; i < rows; ++i) {
        matrix[i] = new float[columns];
        for (unsigned int j = 0; j < columns; ++j) {
            matrix[i][j] = 0.0f;
        }
    }
    return matrix;
}

float det(float** matrix,
          unsigned int columns,
          unsigned int rows)
{
    if (columns != rows) {
        return 0;
    }

    float result;

    if (columns == 1) {
        result = matrix[0][0];
    }
    else if (columns == 2) {
        result = matrix[0][0] * matrix[1][1] - matrix[0][1] * matrix[1][0];
    }
    else {
        for (unsigned int j = 0; j < columns; j++) {
            float** minor;
            minor = create_matrix(columns - 1, rows - 1);
            for (unsigned int y = 0; y < rows - 1; ++y) {
                int k = 0;
                for (unsigned int x = 0; x < columns - 1; ++x) {
                    if (x == j) {
                        k = 1;
                    }
                    minor[y][x] = matrix[y + 1][x + k];
                }
            }
            switch (j % 2) {
                case 0:
                    result += matrix[0][j] * det(minor, columns - 1, rows - 1);
                    break;
                case 1:
                    result += (-matrix[0][j]) * det(minor, columns - 1, rows - 1);
                    break;
            }
        }
    }
    return result;
}
float** algebraic_matrix(float** matrix,
                         unsigned int columns,
                         unsigned int rows)
{
    float** result;
    result = create_matrix(columns, rows);
    for (unsigned int j = 0; j < rows; j++) {
        for (unsigned int i = 0; i < columns; i++) {
            float** minor;
            minor = create_matrix(columns - 1, rows - 1);
            int k1 = 0;
            for (unsigned int y = 0; y < rows - 1; ++y) {
                int k = 0;
                for (unsigned int x = 0; x < columns - 1; ++x) {
                    if (x == i) {
                        k = 1;
                    }
                    if (y == j) {
                        k1 = 1;
                    }

                    minor[y][x] = matrix[y + k1][x + k];
                }
            }
            switch ((j + i) % 2) {
                case 0:
                    result[j][i] = det(minor, columns - 1, rows - 1);
                    break;
                case 1:
                    result[j][i] = (-det(minor, columns - 1, rows - 1));
                    break;
            }
        }
    }
    return result;
}
float** sum(float** matrix1,
            unsigned int columns1,
            unsigned int rows1,
            float** matrix2,
            unsigned int columns2,
            unsigned int rows2,
            unsigned int& newcolumns,
            unsigned int& newrows)
{
    float** result;
    if (columns1 != columns2 || rows1 != rows2) {
        result = NULL;
        return result;
    }
    result = create_matrix(columns1, rows1);
    for (unsigned int j = 0; j < rows1; j++) {
        for (unsigned int i = 0; i < columns1; i++) {
            result[j][i] = matrix1[j][i] + matrix2[j][i];
        }
    }
    newcolumns = columns1;
    newrows = rows1;
    return result;
}
float** sub(float** matrix1,
            unsigned int columns1,
            unsigned int rows1,
            float** matrix2,
            unsigned int columns2,
            unsigned int rows2,
            unsigned int& newcolumns,
            unsigned int& newrows)
{
    float** result;
    if (columns1 != columns2 || rows1 != rows2) {
        result = NULL;
        return result;
    }
    result = create_matrix(columns1, rows1);
    for (unsigned int j = 0; j < rows1; j++) {
        for (unsigned int i = 0; i < columns1; i++) {
            result[j][i] = matrix1[j][i] + matrix2[j][i];
        }
    }

    newcolumns = columns1;
    newrows = rows1;
    return result;
}
float** mul(float** matrix1,
            unsigned int columns1,
            unsigned int rows1,
            float** matrix2,
            unsigned int columns2,
            unsigned int rows2,
            unsigned int& newcolumns,
            unsigned int& newrows)
{
    float** result;
    if (columns1 != rows2) {
        result = NULL;
        return result;
    }
    result = create_matrix(columns2, rows1);
    for (unsigned int j = 0; j < rows1; j++) {
        for (unsigned int i = 0; i < columns2; i++) {
            float y = 0;
            for (unsigned int z = 0; z < columns1; z++) {
                y += matrix1[j][z] * matrix2[z][i];
            }
            result[j][i] = y;
        }
    }
    newcolumns = columns2;
    newrows = rows1;
    return result;
}
float** transplate(float** matrix,
                   unsigned int columns,
                   unsigned int rows,
                   unsigned int& newcolumns,
                   unsigned int& newrows)
{
    float** result;
    result = create_matrix(rows, columns);

    for (unsigned int j = 0; j < columns; j++) {
        for (unsigned int i = 0; i < rows; i++) {
            result[j][i] = matrix[i][j];
        }
    }
    newcolumns = rows;
    newrows = columns;
    return result;
}
float** reverse(float** matrix,
                unsigned int columns,
                unsigned int rows,
                unsigned int& newcolumns,
                unsigned int& newrows)
{
    float** result;
    if (det(matrix, columns, rows) == 0) {

        result = NULL;
        return result;
    }
    if(columns==rows&&rows==1){
        result=NULL;
        return result;
    }
    result = create_matrix(columns, rows);
    float** a = algebraic_matrix(matrix, columns, rows);

    a = transplate(a, columns, rows, newcolumns, newrows);
    float d=det(matrix,columns,rows);
    for (unsigned int j = 0; j < rows; j++) {
        for (unsigned int i = 0; i < columns; i++) {
            result[j][i] = a[j][i] / d;
        }
    }
    newcolumns = columns;
    newrows = rows;
    return result;
}
bool get_matrix(ifstream matrfile,
                float**& matrix,
                unsigned int ncolumns,
                unsigned int nrows)
{
    matrix = create_matrix(ncolumns, nrows);

    for (unsigned int j = 0; j < nrows; j++) {
        string new_row;
        getline(matrfile, new_row);
        istringstream stream(new_row);
        for (unsigned int i = 0; i < ncolumns; i++) {
            if (!(stream >> matrix[j][i])) {
                destroy(matrix,nrows);
                return false;
            }
        }
    }
    return true;
}

void cout_matrix(float** matrix,
                 unsigned int ncolumns,
                 unsigned int nrows)
{

    for (unsigned int j = 0; j < nrows; j++) {
        for (unsigned int i = 0; i < ncolumns; i++) {
            if (matrix[j][i] == -0) {
                matrix[j][i] = 0;
            }
            cout << matrix[j][i] << "\t";
        }
        cout << "\n";
    }
}
bool get_size(ifstream matrfile,
              unsigned int& columns,
              unsigned int& rows)
{
    string header;

    char razdel;
    getline(matrfile, header);
    istringstream str(header);
    if ((str >> rows) && (str >> razdel) && (str >> columns) && (razdel == ',')) {
        return true;
    }
    return false;
}
 bool getfilename(ifstream& fs){
	
	string fn;
	getline(cin,fn);
	ofstream result1(fn);
	
	result1<<"gdfghfh\n";
	result1.close();
	ifstream result(fn);
    return result;
	
	
	
	

	
}
int main()
{
	ifstream m1;
	ifstream m2;
    float** matrix1;
    float** matrix2;
    float** matrix3;
    
   
    
    m1=getfilename();
    char op;
    unsigned int columns1, rows1, columns2, rows2, columns3, rows3;
    if (get_size(columns1, rows1) && get_matrix(matrix1, columns1, rows1)) {
        string com;

        getline(cin, com);
        istringstream stream(com);
        if (stream >> op) {
            switch (op) {
                case 'T':
                    matrix3 = transplate(matrix1, columns1, rows1, columns3, rows3);
                    break;
                case 'R':
                    matrix3 = reverse(matrix1, columns1, rows1, columns3, rows3);
                    break;
                default:
                    if (op != '+' && op != '-' && op != '*') {
                        cout << "An error has occured while reading input data.";
                        exit(0);
                    }
                    break;
            }
            if (matrix3 != NULL && (op == 'T' || op == 'R')) {
                cout_matrix(matrix3, columns3, rows3);
             
                destroy(matrix3,rows3);
                destroy(matrix1,rows1);
                exit(0);
            }
            else if (matrix3 == NULL) {
                cout << "There is no reverse matrix.";

                destroy(matrix1,rows1);
                exit(0);
            }
        }
        else {
            cout << "An error has occured while reading input data.";
            exit(0);
        }
    }
    else {
        cout << "An error has occured while reading input data.";
        exit(0);
    }
    if (get_size(columns2, rows2) && get_matrix(matrix2, columns2, rows2)) {
        switch (op) {
            case '+':
                matrix3 = sum(matrix1, columns1, rows1, matrix2, columns2, rows2, columns3, rows3);
                break;
            case '-':
                matrix3 = sub(matrix1, columns1, rows1, matrix2, columns2, rows2, columns3, rows3);
                break;
            case '*':
                matrix3 = mul(matrix1, columns1, rows1, matrix2, columns2, rows2, columns3, rows3);
                break;
        }
        if (matrix3 != NULL) {
            cout_matrix(matrix3, columns3, rows3);
            destroy(matrix3,rows3);
            destroy(matrix1,rows1);
            destroy(matrix2,rows2);
        }
        else if (matrix3 == NULL) {
            cout << "Wrong matrixes";

            destroy(matrix2,rows2);
            destroy(matrix1,rows1);
            exit(0);
        }
    }
    else {
        cout << "An error has occured while reading input data.";
        exit(0);
    }

}
