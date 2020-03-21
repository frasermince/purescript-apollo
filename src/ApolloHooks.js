var hooks = require('@apollo/react-hooks');
var boost = require('apollo-boost');

exports._gql = boost.gql;
exports._useApolloClient = hooks.useApolloClient;
exports._useQuery = hooks.useQuery;
exports._useMutation = function(q, opts) {
  const [fn, state] = hooks.useMutation(q, opts);
  newFN = (x) => {debugger; return fn(x)};
  return [newFN, state];
}

exports.runThisFn1 = function(key){
  return function(self){
    return function(a){
      return self[key](a);
    }
  }
}
