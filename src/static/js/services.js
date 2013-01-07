'use strict';

/* Services */


// Demonstrate how to register services
// In this case it is a simple value service.
angular.module('ticket.services', ['ngResource']).
  factory('FreeHour', function($resource){
      return $resource('/freehour/:time', {}, {
          query: {
                     method:'GET',
                     params:{time: 'time'}, 
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
  factory('NewLesson', function($resource) {
      return $resource('/lesson/', {}, {
          add: {
                   method: 'POST',
                   isArray: false
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
                }
      });
  });
