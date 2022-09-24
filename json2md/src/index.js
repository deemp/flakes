#!/usr/bin/env node

import json2md from "json2md"
import * as fs from "fs"

var args = process.argv.slice(2)
var input = args[0]

fs.readFile(`${input}`, 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    console.log(json2md(JSON.parse(data)));
});