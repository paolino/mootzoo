

{
Array.prototype.shuffle = function() {
	var input = this;
	
	for (var i = input.length-1; i >=0; i--) {
	
		var randomIndex = Math.floor(Math.random()*(i+1)); 
		var itemAtIndex = input[randomIndex]; 
		
		input[randomIndex] = input[i]; 
		input[i] = itemAtIndex;
	}
	return input;
} 
}
{
var app = angular.module('mootzoo', []);
app.author=false;
app.logged = false;
app.unused=false;
app.directive('ngLeft', function () {
    return function (scope, element, attrs) {
        element.bind("keydown keypress", function (event) {
            if(event.which===37) {
                scope.$apply(function (){
                    scope.$eval(attrs.ngLeft);
                });

                event.preventDefault();
            }
        });
    };
});
app.directive('ngRight', function () {
    return function (scope, element, attrs) {
        element.bind("keydown keypress", function (event) {
            if(event.which===39) {
                scope.$apply(function (){
                    scope.$eval(attrs.ngRight);
                });

                event.preventDefault();
            }
        });
    };
});

app.directive('ngMouseWheelDown', function() {
        return function(scope, element, attrs) {
            element.bind("DOMMouseScroll mousewheel onmousewheel", function(event) {
                        // cross-browser wheel delta
                        var event = window.event || event; // old IE support
                        var delta = Math.max(-1, Math.min(1, (event.wheelDelta || -event.detail || -event.originalEvent.detail)));
                
                        if(delta < 0) {
                            scope.$apply(function(){
                                scope.$eval(attrs.ngMouseWheelDown);
                            });
                        
                            // for IE
                            event.returnValue = false;
                            // for Chrome and Firefox
                            if(event.preventDefault)  {
                                event.preventDefault();
                            }

                        }
            });
        };
});
app.directive('ngMouseWheelUp', function() {
        return function(scope, element, attrs) {
            element.bind("DOMMouseScroll mousewheel onmousewheel", function(event) {
                   
                        // cross-browser wheel delta
                        var event = window.event || event; // old IE support
                        var delta = Math.max(-1, Math.min(1, (event.wheelDelta || -event.detail || -event.originalEvent.detail)));
                
                        //alert(event.originalEvent.detail);                   
                        if(delta > 0) {
                            scope.$apply(function(){
                                scope.$eval(attrs.ngMouseWheelUp);
                            });
                        
                            // for IE
                            event.returnValue = false;
                            // for Chrome and Firefox
                            if(event.preventDefault) {
                                event.preventDefault();                        
                            }

                        }
            });
        };
});
app.controller('conversations', function($scope, $http,$timeout,$interval) {
    newc = function (){$scope.conversations.push({id:Math.floor((Math.random() * 10000000000000) + 1),color:"Blank",
                        index:$scope.conversations.length,messages:[],votes:[],voted:false,prenoted:"free"})};
    
    $scope.setn=function(i){
        $scope.n=i;
        //$scope.bottomed=false;  
        if($scope.conversations.length < 1)newc();
        $timeout(function () {$('#conversation').animate({scrollTop:1000000});});
        }
    $scope.logout=function(){ };

    $scope.conversations=Array();
    $scope.news=Array();
    $scope.message="";
    $scope.setn(0);
    $scope.logged=false;
    $scope.npiu=function(){
                if($scope.n < $scope.conversations.length-1){
                        $scope.setn($scope.n+1);
                        }
        };
    $scope.nmeno=function(){
                        if(($scope.n > 0)){
                        $scope.setn($scope.n-1);
                }
        };

    $scope.login=function(){
        $http.get("../api/GetStore/"+$scope.userkey).success(function(response) {
                var getMessages = function(i) {$http.get("../api/GetMessages/" + response.result[i].mid + "/100").success (
                                function(messages) {
                                        for(var j=0;j<messages.result.length;j++){
                                                $scope.conversations[i].messages.push(messages.result[j]);
                                                }
                                        });}
                var getVote = function(i) {$http.get("../api/GetMessages/" + response.result[i].mid + "/1").success (
                                function(messages) {$scope.conversations[i].messages[0].vote = messages.result[0].vote;}
                                );}
                for (i=0;i < response.result.length;i ++){
                        if($scope.conversations.length-1 < i){
                                $scope.conversations.push(response.result[i]);
                                $scope.conversations[i].index=i;
                                $scope.conversations[i].messages=Array();
                                getMessages (i);
                                }
                        else if (($scope.conversations[i].color !== response.result[i].color) 
                                        || ($scope.conversations[i].mid !== response.result[i].mid)
                                         || ($scope.conversations[i].voted !== response.result[i].voted)
                                          || ($scope.conversations[i].cid !== response.result[i].cid)
                                                ){ 
                                $scope.conversations[i]=response.result[i];
                                $scope.conversations[i].index=i;
                                $scope.conversations[i].messages=Array();
                                getMessages (i);
                                }
                        else getVote(i);
                                
                        }        
                for(j=$scope.conversations.length;j>i;j--)$scope.conversations.pop();
                });
                $scope.logged=true;
        };

    $scope.showFork=function(i){return (!$scope.conversations[$scope.n].messages[i].retr) && (!$scope.isLast(i));}
        
    $scope.positionText=function(){
                switch($scope.conversations[$scope.n].color) {
                        case 'Blank':if($scope.conversations[$scope.n].messages.length < 1) 
                                        return "you can start a new conversation";
                                        return "you can restart this conversation";
                                     
                        case 'Azur':return "you proposed, anyone can respond"
                        case 'Red':return "you can respond, among others"
                        case 'Green':return "you can only read"
                        case 'Blue':return "you responded, only one can respond"
                        case 'Yellow':return "only you can respond"
                }
                }
        
    $scope.positionLabelClass=function(){
                switch($scope.conversations[$scope.n].color) {
                        case 'Blank':return "label label-default"
                        case 'Azur':return "label label-info"
                        case 'Red':return "label label-danger"
                        case 'Green':return "label label-success"
                        case 'Blue':return "label label-primary"
                        case 'Yellow':return "label label-warning"

                }
                }
    $scope.testLeave=function(){return ($scope.conversations[$scope.n].color == 'Yellow')};
    $scope.testVote=function(i){
                if($scope.conversations[$scope.n].voted)return false;
                var c = $scope.isLast(i);
                switch($scope.conversations[$scope.n].color) {
                        case 'Blank':return false;
                        case 'Azur':return false
                        case 'Red':return c
                        case 'Green':return c
                        case 'Blue':return false
                        case 'Yellow':return c
                }
                }
                        
    $scope.testUndo=function(){
                switch($scope.conversations[$scope.n].color) {
                        case 'Blank':return false
                        case 'Azur':return true
                        case 'Red':return false
                        case 'Green':return false
                        case 'Blue':return true 
                        case 'Yellow':return false
                }
                }
    $scope.retractColor=function(){
                switch($scope.conversations[$scope.n].color) {
                        case 'Azur':return "btn-default"
                        case 'Blue':return "btn-danger"
                }
                }
    $scope.retractMeaning=function(){
                 
                switch($scope.conversations[$scope.n].color) {
                        case 'Azur':return "Delete your message and its feedback leaving the conversation with no partecipants"
                        case 'Blue':return "Delete your message and its feedback leaving the conversation open to be taken from others"
                }
                }
    $scope.price=function(n){
                return Math.sqrt(n).toFixed(2);
                }
    $scope.backColor=function(){
        switch($scope.conversations[$scope.n].color) {
                case 'Azur': return "btn btn-default";
                case 'Blue': return "btn btn-danger";
                case 'Yellow':return "btn btn-danger";
                }
        };

    $scope.panelColor=function(){
        switch($scope.conversations[$scope.n].color) {
                case 'Azur': return "panel panel-info";
                case 'Green': return "panel panel-success";
                case 'Blue': return "panel panel-primary";
                case 'Yellow':return "panel panel-warning";
                case 'Red':return "panel panel-danger";
                case 'Blank':return "panel panel-default";
                }
        };
                        
    $scope.isLast=function(i){
        return $scope.conversations[$scope.n].mid==$scope.conversations[$scope.n].messages[i].mid
        }
    $scope.glyphicon=function(n){
        switch($scope.conversations[n].color){
                case 'Azur': return "glyphicon glyphicon-question-sign";
                case 'Green': return "glyphicon glyphicon-transfer";
                case 'Blue': return "glyphicon glyphicon-arrow-down";
                case 'Yellow':return "glyphicon glyphicon-arrow-up";
                case 'Red':return "glyphicon glyphicon-log-in";
                case 'Blank':return "glyphicon glyphicon-unchecked";
                }
        };

               
      
    $scope.convButtonSelected= function(x){
             return ($scope.n==x.index); 
        }
      
    $scope.convButton= function(x){
                switch(x.color) {
                        case 'Azur': return "btn-info";
                        case 'Green': return "btn-success";
                        case 'Blue': return "btn-primary";
                        case 'Yellow':return "btn-warning";
                        case 'Red':return "btn-danger";
                        case 'Blank':return "btn-default";
             };
             };
    $scope.selected= function(i){
        if(i==$scope.n)
                switch($scope.conversations[$scope.n].color) {
                        case 'Azur': return "btn-info";
                        case 'Green': return "btn-success";
                        case 'Blue': return "btn-primary";
                        case 'Yellow':return "btn-warning";
                        case 'Red':return "btn-danger";
                        case 'Blank':return "btn-default";
                        }
                else return "btn-grey"
        }
    $scope.scrollDown=function(){
        $( "#conversation" ).scrollTop(100000);
        }
    //voting stuff
    $scope.voteUp=function(i){
        $http.put("../api/VoteMessage/"+$scope.userkey + "/" +  $scope.conversations[$scope.n].messages[i].mid + "/True").success(
                function () {$scope.login()});
        };

    $scope.voteDown=function(i){
        $http.put("../api/VoteMessage/"+$scope.userkey + "/" +  $scope.conversations[$scope.n].messages[i].mid + "/False").success(
                function () {$scope.login()});
        };

    // messaging stuff
    $scope.respond=function(){
        if($scope.message != ""){
                $http.post("../api/NewMessage/"+$scope.userkey + "/AttachConversation/" + $scope.conversations[$scope.n].cid,$scope.message).success(
                        function () {$scope.login()});
                $scope.message="";
                }
        };
    $scope.comment = function(i){
        if($scope.message != ""){
                $http.post("../api/NewMessage/"+$scope.userkey + "/AttachMessage/" + $scope.conversations[$scope.n].messages[i].mid,$scope.message).success(
                        function () {$scope.login()});
                $scope.message="";
                }
        };
    $scope.open = function(){
        if($scope.message != ""){
                $http.post("../api/NewMessage/"+$scope.userkey + "/DontAttach",$scope.message).success(
                        function () {$scope.login()});
                $scope.message="";
                }
        };
    $scope.retract = function (){
        $http.put("../api/RetractMessage/"+$scope.userkey + "/" + $scope.conversations[$scope.n].cid).success(
                function () {$scope.login()});
        }
        
    $scope.leave = function (){
        $http.put("../api/LeaveConversation/"+$scope.userkey + "/" + $scope.conversations[$scope.n].cid).success(
                function () {$scope.login()});
        }
    $scope.hint = function (){
        $http.put("../api/HintConversation/"+$scope.userkey ).success(
                function () {$scope.login()});
        }
    $scope.invite=function (){
        alert("Mail broken");
        $http.post("../api/Invite/"+$scope.userkey,$scope.message).success(
                function () {$scope.login()});
        }

    $scope.reminds=function (){
        alert("Mail broken");
        $http.post("../api/Reminds",$scope.message).success(
                function () {$scope.login()});
        }
    $scope.logout=function (){
        alert("Mail broken");
        $http.put("../api/Logout"+$scope.userkey).success(
                function () {$scope.login()});
        }
    $scope.forget= function (){
        $http.put("../api/ForgetConversation/"+$scope.userkey+"/"+$scope.conversations[$scope.n].cid).success(
                function () {$scope.login()});
        }
        
    $timeout(function(){
        if($scope.userkey>0) $scope.login();
        });
    $interval(function(){
        if($scope.userkey>0) $scope.hint();
        },10000);
    $interval(function(){
        if($scope.message=="")location.reload();
        },100000);
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
