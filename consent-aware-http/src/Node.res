// Node.js bindings

module Fs = {
  @module("fs/promises") external readFile: (string, string) => promise<string> = "readFile"
}

module Path = {
  @module("path") external join: (string, string) => string = "join"
}

@val external dateNow: unit => float = "Date.now"
