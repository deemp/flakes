// SPDX-License-Identifier: MIT
pragma solidity ^0.8.17;

contract Database {
    struct Table {
        // column names
        string[] columns;
        // column name -> values
        mapping(string => string[]) values;
        // if a table exists
        bool exists;
    }

    mapping(string => Table) table;

    mapping(string => bool) has;

    function contains(
        string[] memory a,
        string[] memory b
    ) private returns (bool) {
        for (uint i = 0; i < b.length; i++) {
            has[b[i]] = false;
        }
        for (uint i = 0; i < a.length; i++) {
            has[a[i]] = true;
        }
        bool contains_ = true;
        for (uint i = 0; i < b.length; i++) {
            contains_ = contains_ && has[b[i]];
        }
        return contains_;
    }

    function CREATE_TABLE(string memory a, string[] memory b) public {
        table[a].columns = b;
        string[] memory p;
        for (uint i = 0; i < b.length; i++) {
            table[a].values[b[i]] = p;
        }
        table[a].exists = true;
    }

    function DROP_TABLE(string memory a) public {
        delete table[a];
        table[a].exists = false;
    }

    mapping(string => string) toInsert;

    function INSERT(
        string memory tableName,
        string[] memory columns_,
        string[] memory values_
    ) public {
        require(table[tableName].exists, "No such table");
        require(
            columns_.length == values_.length,
            "Mismatched #columns and #values"
        );
        require(
            contains(table[tableName].columns, columns_),
            "No such columns"
        );
        string[] memory tableColumns = table[tableName].columns;
        for (uint i = 0; i < tableColumns.length; i++) {
            toInsert[tableColumns[i]] = "";
        }
        for (uint i = 0; i < columns_.length; i++) {
            toInsert[columns_[i]] = values_[i];
        }
        for (uint i = 0; i < tableColumns.length; i++) {
            table[tableName].values[tableColumns[i]].push(
                toInsert[tableColumns[i]]
            );
        }
    }

    mapping(string => string[]) selectResult;

    event SELECT_EVENT(string[][]);

    function SELECT(
        string memory tableName,
        string[] memory columns_
    ) public returns (string[][] memory) {
        require(table[tableName].exists, "No such table");
        require(
            contains(table[tableName].columns, columns_),
            "Some columns don't exist on the given table"
        );
        uint size = columns_.length;
        string[][] memory res = new string[][](size);
        for (uint i = 0; i < columns_.length; i++) {
            string[] memory v = table[tableName].values[columns_[i]];
            res[i] = v;
        }
        emit SELECT_EVENT(res);
        return res;
    }
}
