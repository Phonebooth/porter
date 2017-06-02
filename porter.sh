#!/bin/bash

function main {
    erl -pa ebin -s porter -config porter
}

main
