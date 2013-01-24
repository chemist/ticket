/* Controllers */

// format date as russian string
Date.prototype.toLocaleDateString = function() {
    var ruDate = {
        days: ["Воскресенье", "Понедельник", "Вторник", "Среда", "Четверг", "Пятница", "Суббота", "Воскресенье"],
        daysShort: ["Вск", "Пнд", "Втр", "Срд", "Чтв", "Птн", "Суб", "Вск"],
        daysMin: ["Вс", "Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Вс"],
        months: ["Января", "Февраля", "Марта", "Апреля", "Мая", "Июня", "Июля", "Августа", "Сентября", "Октября", "Ноября", "Декабря"],
        monthsShort: ["Янв", "Фев", "Мар", "Апр", "Май", "Июн", "Июл", "Авг", "Сен", "Окт", "Ноя", "Дек"]
    };
    var month = ruDate.months[this.getMonth()]; // from haskell month
    var day = ruDate.days[this.getDay()];
    return day + ' ' + this.getDate() + ' ' + month + ' ' + this.getFullYear();

};

function Login($scope, $http) {
    $scope.login = function(data) {
        $http.post('/login', data).success(function(x) {
            var urll = '/#/global/';
            window.location.href = urll;
        }).error(function(x) {
            $(".alert").removeClass("hide")
        });
    };
};

function redirectToLogin(data) {
    window.location.href = "/#/login";
};

function Room($scope, $http, $routeParams, Lesson, Guest) {
    $scope.lesson = Lesson.get({
        id: $routeParams.id
    }, function() {}, function(data) {
        redirectToLogin(data);
    });
    // trigger for green | red class in css
    $scope.setColor = function(x) {
        return (x.guest === null) ? "badge badge-warning" : "badge badge-success";
    };

    // add guest to free room
    $scope.addGuest = function(data) {
        console.log($scope.form.roomId);
        if ($scope.form.roomId) {
            var freeRoom = {
                roomId: $scope.form.roomId,
                guest: null
            }
        } else {
            var freeRoom = $scope.lesson.rooms.filter(function(x) {
                return x.guest === null;
            })[0]
        };
        var guest = {
            firstname: data.firstname ? data.firstname : "",
            secondname: data.secondname ? data.secondname : "",
            age: data.age ? data.age : 0,
            phone: data.phone ? data.phone : "",
            comment: data.comment ? data.comment : "",
            guestid: 0
        };
        Guest.add({}, guest, function(res) {
            if (freeRoom) {
                var newRoom = {
                    roomId: freeRoom.roomId,
                    guest: {
                        gid: res.guestid
                    }
                };
                $scope.lesson.rooms[freeRoom.roomId - 1] = newRoom;

                Lesson.post({
                    id: $routeParams.id
                }, $scope.lesson, function(data) {
                    $scope.lesson = data;
                    console.log(data);
                });
                $scope.form.roomId = undefined

            } else {
                console.log('max allowed room')
            };
        });
    };

    // remove guest from room
    $scope.cleanRoom = function(data) {
        var newRooms = $scope.lesson.rooms.map(function(y) {
            return y.roomId != data.roomId ? y : {
                roomId: y.roomId,
                guest: null
            };
        });
        $scope.lesson.rooms = newRooms;
        Lesson.post({
            id: $routeParams.id
        }, $scope.lesson);
    };

    // edit room
    $scope.editRoom = function(data) {
        $scope.form.firstname = data.guest.firstname;
        $scope.form.secondname = data.guest.secondname;
        $scope.form.phone = data.guest.phone;
        $scope.form.age = data.guest.age;
        $scope.form.comment = data.guest.comment;
        $scope.form.roomId = data.roomId;
        var edit = '#list_' + data.roomId;
        $(edit).addClass('edit');
    };

};

function Global($scope, FreeHour, Lessons, Lesson, $location) {
    $scope.go = function(url) {
        var urll = '/#/lesson/' + url;
        window.location.href = urll;
    };
    $scope.lessonForm = {
        utcTime: new Date()
    };
    // info scope
    $scope.loadInfo = function() {
        $scope.info = Lessons.query({
            time: $scope.lessonForm.utcTime.toISOString()
        }, function() {}, function(x) {
            redirectToLogin(x);
        });
    };
    $scope.showDate = function(x) {
        var date = new Date(x);
        return date.toLocaleDateString();
    };
    $scope.showTime = function(x) {
        var date = new Date(x);
        return date.getHours() + ':' + date.getMinutes();
    };

    $scope.freeRooms = function(x) {
        return x.filter(function(y) {
            return y.guest === null;
        }).length;
    };
    $scope.closeRooms = function(x) {
        return x.filter(function(y) {
            return y.guest != null;
        }).length;
    };

    $scope.loadInfo();

    // format for datepecker
    $scope.dateoptions = {
        format: 'dd-mm-yyyy',
        language: 'ru'
    };
    $scope.timeoptions = {
        defaultTime: 'current',
        showMeridian: false,
        showInputs: false,
        disableFocus: true
    };

    // add lesson
    $scope.addLesson = function(x) {
        $scope.lessonForm.utcTime.setHours(x.utime.hour);
        $scope.lessonForm.utcTime.setMinutes(x.utime.minute);
        var n = {
            date: $scope.lessonForm.utcTime,
            lessonId: 0,
            rooms: [],
            classroom: parseInt(x.classroom),
            teacher: x.teacher
        };
        Lesson.add(n, function(data) {
            $scope.info.push(data);
        });
    };

};
