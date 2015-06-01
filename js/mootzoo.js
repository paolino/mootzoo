

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
app.controller('conversations', function($scope, $http) {
    newc = function (){$scope.convers.push({id:Math.floor((Math.random() * 10000000000000) + 1),type:"blank",
                        index:$scope.convers.length,messages:[],votes:[],voted:false,prenoted:"free"})};
    
    
    $scope.logout=function(){ 
    $scope.convers=Array();
    $scope.conversations=$scope.convers;
    $scope.news=Array();
    $scope.hints=Array();
    $scope.message="Message..";
            $scope.n=0;
            $scope.logged=false;
            $scope.message="";
            };
    $scope.logout();
    $scope.npiu=function(){
                if($scope.n < $scope.conversations.length-1){
                        $scope.n=$scope.n+1;
                        }
        };
    $scope.nmeno=function(){
                        if(($scope.n > 0)){
                        $scope.n=$scope.n-1;
                }
        };
    $scope.filterBlanks = function(){
        for(i=0,j=0;i<$scope.convers.length;i ++)
                if ($scope.convers[i].type != 'blank'){
                        $scope.convers[j] = $scope.convers[i];
                        $scope.convers[j].index=j++;
                        }
        for(;j<i;j++) $scope.convers.pop();
        $scope.n=0;
        }

    $scope.login=function(){
        $http.get("conversations.json").success(function(response) {
                var j = $scope.convers.length;
                for (i=0;i < response.length;i ++){
                        response[i].index=j++
                        $scope.convers.push(response[i]);
                        $scope.news.push(response[i]);
                        if((response[i].type != 'conversata') && (response[i].type != 'personale') && (response[i].type != 'waiting'))
                                $scope.hints.push(response[i]);
                        }
                        
                $scope.news.shuffle();
                $scope.filterBlanks();
        });
        $scope.logged=true;
        };
    $scope.jump=function(where){
        };
    $scope.positionText=function(){
                switch($scope.conversations[$scope.n].type) {
                        case 'blank':return "you can propose"
                        case 'personale':return "you proposed, anyone can respond"
                        case 'orphan':return "you can respond, among others"
                        case 'complete':return "you can only read"
                        case 'conversata':return "you responded, only one can respond"
                        case 'waiting':return "only you can respond"
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
        $scope.filterBlanks();
        };
    $scope.backPresent=function(){return ($scope.conversations[$scope.n].type == 'waiting')};
    $scope.testVote=function(i){
                if($scope.conversations[$scope.n].voted)return false;
                switch($scope.conversations[$scope.n].type) {
                        case 'blank':return false;
                        case 'personale':return false
                        case 'orphan':return (i == $scope.conversations[$scope.n].messages.length - 1)
                        case 'complete':return (i == $scope.conversations[$scope.n].messages.length - 1)  
                        case 'conversata':return false
                        case 'waiting':return (i == $scope.conversations[$scope.n].messages.length - 1)
                }
                }
                        
    $scope.voteUp=function(i){
                if(!$scope.conversations[$scope.n].voted[i])
                        $scope.conversations[$scope.n].votes[i]+=1;
                $scope.conversations[$scope.n].voted=true;
                }

    $scope.voteDown=function(i){
                if(!$scope.conversations[$scope.n].voted[i])
                        $scope.conversations[$scope.n].votes[i]-=1;
                $scope.conversations[$scope.n].voted=true;
                }
    $scope.testUndo=function(){
                switch($scope.conversations[$scope.n].type) {
                        case 'blank':return false
                        case 'personale':return true
                        case 'orphan':return false
                        case 'complete':return false
                        case 'conversata':return true 
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

               
      
        $scope.convButtonSelected= function(x){
             return ($scope.n==x.index); 
        }
      
        $scope.convButton= function(x,i){
             var s = ""
             if (i==$scope.n) s=""; 
             if(x.type=="complete")
                    return "btn btn-xs btn-success" + s
             else if(x.type=="orphan")
                    return "btn btn-xs btn-danger"+ s
             else if(x.type=='waiting')
                    return "btn btn-xs btn-warning"+ s
             else if(x.type=='conversata')
                    return "btn btn-xs btn-primary"+ s
             else if(x.type=='personale')
                    return "btn btn-xs btn-info"+ s
             else return "btn btn-xs btn-default"+ s
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
    $scope.scrollDown=function(){
        $( "#conversation" ).scrollTop(100000);
        }
    $scope.stepIn = function(){
        $scope.filterBlanks();
                        
        newc();
        c = $scope.convers[$scope.convers.length - 1];
        c.type='personale'
        c.messages.push($scope.message);
        c.votes.push(0);
        c.voted=true;
        $scope.n = c.index;
        $scope.message=null;
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
