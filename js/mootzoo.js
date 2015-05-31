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
app.controller('conversations', function($scope, $http) {
    $scope.conversations=Array();
    $scope.n=0;
    var i;


    newc = function (){return  {id:Math.floor((Math.random() * 10000000000000) + 1),type:"blank",messages:[],votes:[],voted:[],prenoted:"free"}};

    for(i=0;i < 90;i ++){
        $scope.conversations.push(newc());
        }
    $scope.message="";
    $scope.npiu=function(){
                if($scope.n < (90-1)){
                        $scope.n=$scope.n+1;
                        }
        };
    $scope.nmeno=function(){
                        if(($scope.n > 0)){
                        $scope.n=$scope.n-1;
                }
        };
    $scope.login=function(){
        $http.get("conversation.json").success(function(response) {
                for (i=0;i < response.length;i ++)
                $scope.conversations[i]=response[i];
        });
        $scope.logged=true;
        };
    $scope.jump=function(where){
        $http.get("conversations.json").success(function(response) {
                var i=0;
                for(;i < response.length; i ++)
                        if (response[i].id==where){
                                      $scope.conversations[$scope.n]=response[i];
                                      break;
                                      }
                if(i >= response.length) alert("Conversation not found");
                });
        };
    $scope.positionText=function(){
                switch($scope.conversations[$scope.n].type) {
                        case 'blank':return "no one is partecipating"
                        case 'personale':return "you are waiting for a partecipant"
                        case 'orphan':return "you can partecipate"
                        case 'complete':return "two others are partecipating"
                        case 'conversata':return "you are waiting for a response"
                        case 'waiting':return "you are asked to respond"
                }
                }
        
    $scope.positionLabelClass=function(){
                switch($scope.conversations[$scope.n].type) {
                        case 'blank':return "label label-default"
                        case 'personale':return "label label-info"
                        case 'orphan':return "label label-danger"
                        case 'complete':return "label label-success"
                        case 'conversata':return "label label-primary"
                        case 'waiting':return "label label-warning"

                }
                }
    $scope.logout=function(){ 
                $scope.conversations=Array();
                for (i=0;i < 90;i ++)
                        $scope.conversations.push(newc());
                $scope.logged=false;
                };
    $scope.cantBeLost=function(){return ($scope.conversations[$scope.n].type == 'waiting')
                || ($scope.conversations[$scope.n].type == 'conversata') };
    $scope.back=function(){
        switch($scope.conversations[$scope.n].type) {
                case 'personale': 
                                $scope.conversations[$scope.n].messages.pop();
                                $scope.conversations[$scope.n].type="blank";
                                break;
                case 'conversata': 
                                $scope.conversations[$scope.n].messages.pop();
                                $scope.conversations[$scope.n].type="orphan";
                                break;
                case 'waiting':
                                
                                $scope.conversations[$scope.n].type="orphan";
                                break;
                }
        };
    $scope.backPresent=function(){return ($scope.conversations[$scope.n].type == 'waiting')};
    $scope.testVote=function(i){
                switch($scope.conversations[$scope.n].type) {
                        case 'blank':return false;
                        case 'personale':return false
                        case 'orphan':return (i == $scope.conversations[$scope.n].messages.length - 1)
                        case 'complete':return (i == $scope.conversations[$scope.n].messages.length - 1) || 
                                        (i == $scope.conversations[$scope.n].messages.length - 2)
                        case 'conversata':return false
                        case 'waiting':return (i == $scope.conversations[$scope.n].messages.length - 1)
                }
                }
                        
    $scope.voteUp=function(i){
                if(!$scope.conversations[$scope.n].voted[i])
                        $scope.conversations[$scope.n].votes[i]+=1;
                $scope.conversations[$scope.n].voted[i]=true;
                }

    $scope.voteDown=function(i){
                if(!$scope.conversations[$scope.n].voted[i])
                        $scope.conversations[$scope.n].votes[i]-=1;
                $scope.conversations[$scope.n].voted[i]=true;
                }
    $scope.testUndo=function(i){
                switch($scope.conversations[$scope.n].type) {
                        case 'blank':return false;
                        case 'personale':return (i == $scope.conversations[$scope.n].messages.length - 1)
                        case 'orphan':return false
                        case 'complete':return false
                        case 'conversata':return (i == $scope.conversations[$scope.n].messages.length - 1)
                        case 'waiting':return false
                }
                }

    $scope.backColor=function(){
        switch($scope.conversations[$scope.n].type) {
                case 'personale': return "btn btn-default";
                case 'conversata': return "btn btn-danger";
                case 'waiting':return "btn btn-danger";
                }
        };

    $scope.panelColor=function(){
        switch($scope.conversations[$scope.n].type) {
                case 'personale': return "panel panel-info";
                case 'complete': return "panel panel-success";
                case 'conversata': return "panel panel-primary";
                case 'waiting':return "panel panel-warning";
                case 'orphan':return "panel panel-danger";
                case 'blank':return "panel panel-default";
                }
        };
                                
    $scope.glyphicon=function(n){
        switch($scope.conversations[n].type){
                case 'personale': return "glyphicon glyphicon-question-sign";
                case 'complete': return "glyphicon glyphicon-transfer";
                case 'conversata': return "glyphicon glyphicon-arrow-down";
                case 'waiting':return "glyphicon glyphicon-arrow-up";
                case 'orphan':return "glyphicon glyphicon-log-in";
                case 'blank':return "glyphicon glyphicon-unchecked";
                }
        };

               
      
        $scope.convButtonSelected= function(i){
             return (i==$scope.n); 
        }
      
        $scope.convButton= function(x,i){
             var s = ""
             if (i==$scope.n) s=""; 
             if(x.type=="complete")
                    return "btn btn-success" + s
             else if(x.type=="orphan")
                    return "btn btn-danger"+ s
             else if(x.type=='waiting')
                    return "btn btn-warning"+ s
             else if(x.type=='conversata')
                    return "btn btn-primary"+ s
             else if(x.type=='personale')
                    return "btn btn-info"+ s
             else return "btn btn-default"+ s
             };
    $scope.removeConversation=function(){
        $scope.conversations[$scope.n].id=Math.floor((Math.random() * 10000000000000) + 1);
        $scope.conversations[$scope.n].type="blank";
        $scope.conversations[$scope.n].messages=[];
        };
    $scope.addMessage=function(){
        $scope.conversations[$scope.n].messages.push($scope.message);
        $scope.conversations[$scope.n].votes.push(0);
        $scope.message="";
        };
    $scope.switchRole=function(){
        $scope.conversations[$scope.n].messages.push($scope.message);
        $scope.message="";
        };
    $scope.selected= function(i){
        if(i==$scope.n)
                switch($scope.conversations[$scope.n].type) {
                        case 'personale': return "btn-info";
                        case 'complete': return "btn-success";
                        case 'conversata': return "btn-primary";
                        case 'waiting':return "btn-warning";
                        case 'orphan':return "btn-danger";
                        case 'blank':return "btn-default";
                        }
                else return "btn-grey"
        }
   $scope.canPrenote=function(){
       return ($scope.conversations[$scope.n].prenoted=="free")
        }
   $scope.canUnprenote=function(){
       return ($scope.conversations[$scope.n].prenoted=="you")
        }
   $scope.prenote=function(){
       $scope.conversations[$scope.n].prenoted="you";
        }

   $scope.unprenote=function(){
       $scope.conversations[$scope.n].prenoted="free";
        }
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
