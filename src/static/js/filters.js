'use strict';

/* Filters */

angular.module('ticket.filters', []).
filter('removeNull', function() {
	return function(input) {
		var t = [];
		for (var i in input) {
			if (input[i].guest != null ) {
				t.push(input[i]);
			};
		};
		return t;
	};
});
