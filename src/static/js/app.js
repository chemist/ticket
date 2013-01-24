
// Declare app level module which depends on filters, and services
angular.module('ticket', ['ticket.filters', 'ticket.services', 'ticket.directives']).
  config(['$routeProvider', function($routeProvider) {
    $routeProvider.when('/global', {templateUrl: 'template/global.html', controller: Global});
    $routeProvider.when('/lesson/:id', {templateUrl: 'template/room.html', controller: Room});
    $routeProvider.when('/login', {templateUrl: 'template/login.html', controller: Login});
    $routeProvider.otherwise({redirectTo: '/login'});
  }]);



