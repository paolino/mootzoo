{
var app = angular.module('mootzoo', []);
app.author=false;
app.logged = false;
app.unused=false;
app.controller('conversations', function($scope, $http) {
    $http.get("conversations.json").success(function(response) {$scope.conversations = response;});


    $scope.convButton= function(x){
     if(x.type=="complete")
            return "btn btn-success"
     else if(x.type=="orphan")
            return "btn btn-danger"
     else
            return "btn btn-warning"
     };


});
}
    
window.onload = function() {
    var t = document.getElementsByTagName('textarea')[0];
    var offset= !window.opera ? (t.offsetHeight - t.clientHeight) : (t.offsetHeight + parseInt(window.getComputedStyle(t, null).getPropertyValue('border-top-width'))) ;
 
    var resize  = function(t) {
        t.style.height = 'auto';
        t.style.height = (t.scrollHeight  + offset ) + 'px';    
    }
 
    t.addEventListener && t.addEventListener('input', function(event) {
        resize(t);
    });
 
    t['attachEvent']  && t.attachEvent('onkeyup', function() {
        resize(t);
    });
}
