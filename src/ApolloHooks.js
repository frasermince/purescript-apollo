var hooks = require('@apollo/react-hooks');
var boost = require('apollo-boost');

exports._gql = boost.gql;
exports._useQuery = hooks.useQuery;
exports._useMutation = function(node, opts) {
  return function() {
    hooks.useMutation(node, opts);
  }
}
exports.runThisFn1 = function(key){
  return function(self){
    return function(a){
      return self[key](a);
    }
  }
}
