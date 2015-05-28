{
var app = angular.module('mootzoo', []);
app.author=false;
app.logged = false;
app.unused=false;
app.controller('conversations', function($scope, $http) {
    $scope.conversations=[];
    $scope.n=0;
    $scope.message="";
    $http.get("conversations.json").success(function(response) {$scope.conversations = response;});
    $scope.addConversation=function(){
                var m = $scope.conversations.slice(-1)[0].n + 1;
                $scope.conversations.push({n:m,id:Math.floor((Math.random() * 10000000000000) + 1),type:"blank",messages:[]});
                $scope.n=m-11;
                };
    $scope.canBeLost=function(){return ($scope.conversations[$scope.n].type == 'waiting')
                || ($scope.conversations[$scope.n].type == 'conversata') 
                || ($scope.conversations[$scope.n].type == 'personale');};

    $scope.removeConversation=function(){
        $scope.conversations[$scope.n].id=Math.floor((Math.random() * 10000000000000) + 1);
        $scope.conversations[$scope.n].type="blank";
        $scope.conversations[$scope.n].messages=[];
        };
    $scope.addMessage=function(){
        $scope.conversations[$scope.n].messages.push($scope.message);
        $scope.message="";
        };
    $scope.convButton= function(x){
     if(x.type=="complete")
            return "btn btn-success"
     else if(x.type=="orphan")
            return "btn btn-danger"
     else if(x.type=='waiting')
            return "btn btn-warning"
     else if(x.type=='conversata')
            return "btn btn-primary"
     else if(x.type=='personale')
            return "btn btn-info"
     else return "btn btn-default"
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
