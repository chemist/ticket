'use strict';

/* Services */


// Demonstrate how to register services
// In this case it is a simple value service.
angular.module('ticket.services', ['ngResource']).
  factory('Users', function($resource){
      return $resource('/users', {}, {
          get: {
                     method:'GET',
                     isArray:true}
      });

  }).
  factory('Lessons', function($resource){
      return $resource('/lessons/:time', {}, {
          query: {
                   method: 'GET',
                   params: {time: 'time'},
                   isArray:true
                 }
      });
  }).
  factory('Lesson', function($resource) {
      return $resource('lesson/:id', {}, {
          get: {
                   method: 'GET',
                   params: {id: 'id'},
                   isArray: false
               },
          post: {
                    method: 'POST',
                    params: {id: 'id'},
                    isArray: false
                },
          add: {
                   method: 'POST',
                   isArray: false
               }
      });
  }).
  factory('Guest', function($resource) {
      return $resource('/guest/:id', {}, {
          get: {
              method: 'GET',
              params: {id: 'id'},
              isArray: false
          },
          post:{
              method: 'POST',
              params: {id: 'id'},
              isArray: false
          },
          query: {
              method: 'GET',
              isArray: true
          },
          add: {
              method: 'POST',
              isArray: false
          }
      });
  });
