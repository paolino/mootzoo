

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
        $scope.viewing=$scope.conversations[$scope.n];
        }
    $scope.scrolling=Array(); 
    $scope.viewing=null;
    $scope.conversations=Array();
    $scope.news=Array();
    $scope.message=null;
    $scope.setn(0);
    $scope.logged=false;
    $scope.copy=null;
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
    $scope.getMessages = function(conv) {
                $http.get("../api/GetMessages/" + conv.mid + "/100").success (
                                function(messages) {
                                        conv.messages=Array();
                                        for(var j=0;j<messages.result.length;j++){
                                                conv.messages.push(messages.result[j]);
                                                }
                                        });
        }
    $scope.getLast = function(conv) {
                $http.get("../api/GetMessages/" + conv.mid + "/1").success (
                                function(messages) {
                                        conv.messages[0].vote = messages.result[0].vote;
                                        conv.conversations[i].messages[0].txt = messages.result[0].txt;
                                        }
                                );
        }
    $scope.login=function(){
        $http.get("../api/GetStore/"+$scope.userkey).success(function(response) {
                for (i=0;i < response.result.length;i ++){
                        if($scope.conversations.length-1 < i){
                                $scope.conversations.push(response.result[i]);
                                $scope.conversations[i].index=i;
                                $scope.conversations[i].messages=Array();
                                $scope.getMessages (i);
                                }
                        else if (($scope.conversations[i].color !== response.result[i].color) 
                                        || ($scope.conversations[i].mid !== response.result[i].mid)
                                         || ($scope.conversations[i].voted !== response.result[i].voted)
                                          || ($scope.conversations[i].cid !== response.result[i].cid)
                                                ){ 
                                $scope.conversations[i]=response.result[i];
                                $scope.conversations[i].index=i;
                                $scope.conversations[i].messages=Array();
                                $scope.getMessages ($scope.conversations[i]);
                                }
                        else $scope.getLast($scope.conversations[i]);
                                
                        }        
                for(j=$scope.conversations.length;j>i;j--)$scope.conversations.pop();
                });
                $scope.logged=true;
        };

    $scope.showFork=function(i){return (!$scope.conversations[$scope.n].messages[i].retr) && (!$scope.isLast(i));}
        
    $scope.positionText=function(x){
                switch(x.color) {
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
             if ($scope.n==x.index)
                s="btn-md";
             else s="btn-xs"; 
                
                switch(x.color) {
                        case 'Azur': return "btn-info " + s;
                        case 'Green': return "posr btn-success "+ s;
                        case 'Blue': return "btn-primary " +s;
                        case 'Yellow':return "post btn-warning " +s;
                        case 'Red':return "posr btn-danger " +s;
                        case 'Blank':return "posr btn-default " +s;
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
    $scope.setMessage=function(t){
        $scope.message=t;
        }
    $scope.correct=function(){
        if($scope.message){
                $http.post("../api/NewMessage/"+$scope.userkey + "/CorrectConversation/" + $scope.conversations[$scope.n].cid,$scope.message).success(
                        function () {$scope.login()});
                $scope.message=null;
                }
        };

    $scope.respond=function(){
        if($scope.message){
                $http.post("../api/NewMessage/"+$scope.userkey + "/AttachConversation/" + $scope.conversations[$scope.n].cid,$scope.message).success(
                        function () {$scope.login()});
                $scope.message=null;
                }
        };
    $scope.comment = function(i){
        if($scope.message){
                $http.post("../api/NewMessage/"+$scope.userkey + "/AttachMessage/" + $scope.conversations[$scope.n].messages[i].mid,$scope.message).success(
                        function () {$scope.login()});
                $scope.message=null;
                }
        };
    $scope.open = function(){
        if($scope.message){
                $http.post("../api/NewMessage/"+$scope.userkey + "/DontAttach",$scope.message).success(
                        function () {$scope.login()});
                $scope.message=null;
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
        $http.post("../api/Invite/"+$scope.userkey,$scope.mailremainder).success(
                function () {$scope.login();$scope.mailremainder=null;});
        }

    $scope.reminds=function (){
        $http.post("api/Reminds",$scope.mailremainder).success(
                function () {$scope.mailremainder=null});
        }
    $scope.logout=function (){
        $http.put("../api/Logout/"+$scope.userkey).success(
                function () {location.reload();});
        }
    $scope.clean=function(){
        $scope.message=null;
        };
    $scope.canClean=function(){
        return $scope.message
        };
    $scope.copi=function(){
        if($scope.message){
                $scope.copy=$scope.message;
                $scope.message=null;
                }
        };
    $scope.canCopi=function(){
        return $scope.message
        };
    $scope.paste=function(){
        if($scope.copy){
                $scope.message=$scope.copy;
                }
        };
    $scope.canPaste=function(){
        return $scope.copy;
        };
    $scope.search=function(){
        $http.post("../api/GetSearch",$scope.searchValue).success(
                function (rs) {
                        $scope.searched=rs.result;
                        for(var i=0;i<rs.result.length;i++)
                                rs.result[i].index=i;
                        });
        };
    $scope.viewSearch=function(i){
        $scope.getMessages($scope.searched[i]);
        $scope.viewing = $scope.searched[i];
        }
    $scope.forget= function (){
        $http.put("../api/ForgetConversation/"+$scope.userkey+"/"+$scope.conversations[$scope.n].cid).success(
                function () {$scope.login()});
        };
        

    $scope.store = function (ci){
        $http.put("../api/StoreConversation/"+$scope.userkey+"/"+ci).success(
                function () {$scope.login()});
        };
    $timeout(function(){
        if($scope.userkey>0) $scope.login();
        });
    $interval(function(){
        if($scope.userkey>0) $scope.hint();
        },10000);
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
    t['attachEvent']  && t.attachEvent('onclick', function() {
        resize(t);
    });
}
