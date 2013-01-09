/* Directives */


angular.module('ticket.directives', []).
directive('appVersion', ['version', function(version) {
	return function(scope, elm, attrs) {
		elm.text(version);
	};
}]).
directive('bDatepicker', function() {
	return {
		require: '?ngModel',
		restrict: 'A',
		link: function(scope, element, attrs, ngModelCtrl) {
			var originalRender, updateModel;
			updateModel = function() {
                element.datepicker('hide');
                element.blur();
				scope.$apply(function() {
					ngModelCtrl.$setViewValue(element.data().datepicker.date);
				});
			};
			if (ngModelCtrl) {
				originalRender = ngModelCtrl.$render;
				ngModelCtrl.$render = function() {
					originalRender();
					return element.datepicker.date = ngModelCtrl.$viewValue;
				};
			}  
			return attrs.$observe('bDatepicker', function(value) {
				var options;
				options = {};
				if (angular.isObject(value)) {
					options = value;
				}
				if (typeof(value) === "string") {
					options = angular.fromJson(value);
				}
				return element.datepicker(options).on('changeDate', updateModel);
			});
		}
	};
}).
directive('bTimepicker', function() {
    return  {
        restrict: 'A',
        link: function(scope, element, attrs) {
            return attrs.$observe('bTimepicker', function(value) {
                var options;
                options = {};
                if (angular.isObject(value)) {
                    options = value;
                };
                if (typeof(value) === "string") {
                    options = angular.fromJson(value);
                };
                return element.timepicker(options)
            });
        }
    };
});
