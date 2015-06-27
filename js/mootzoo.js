{
var app = angular.module("app",['ui.bootstrap']);
app.controller('Input', function ($scope, $modalInstance) {

          $scope.gotMessage = function () {
            $modalInstance.close();
          };

          $scope.cancel = function () {
            $modalInstance.dismiss('cancel');
          };
        });
app.controller('conversations', function($scope,$timeout,$modal,$log,$http,$interval,$filter) {
        $scope.message="ciao";        
        $scope.input= {};
        $scope.messageid=0;
        $scope.notgetting=1;	
	$scope.stopRefresh=false;
        $scope.setUserkey=function(u) {
                $scope.userkey=u;
                $scope.getConversation($scope.messageid);
                }
        $scope.getOpens=function(){$http.get("../api/Opens/" + $scope.userkey).success(function(xs){
                        $scope.opens=xs.result;
                        });
                }
        $timeout($scope.getOpens);
        $scope.getRoots=function(){$http.get("../api/Roots/" + $scope.userkey).success(function(xs){
                        $scope.roots=xs.result;
                        });
                }
        $timeout($scope.getRoots);
        $scope.getDetti=function(){$http.get("../api/Owned/" + $scope.userkey).success(function(xs){
                        $scope.detti=xs.result;
                        });
                }
        $timeout($scope.getDetti);
        $scope.getPersonal=function(){$http.get("../api/Personal/" + $scope.userkey).success(function(xs){
                        $scope.personal=xs.result;
                        });
                }
        $timeout($scope.getPersonal);
        $scope.lastConversation=Array();
        $scope.past=function(){
                if($scope.conversation[0].parent)
                $scope.getConversation($scope.conversation[0].parent);
                }
        $scope.gotox=function(){
                $scope.getConversation($scope.gotoConversation);
                }
        $scope.select = function (msgs,what) {
		$scope.input.msgs=msgs;
		$scope.input.what=what;
                var modalInstance = $modal.open({
                        animation: true,
                        templateUrl: '../msgs.html',
                        controller: 'Input',
                        size: 'md',
                        scope:$scope
                        });
                modalInstance.result.then(
                        function () {}, 
                        function () {}
                        );
                };

        $scope.open = function (next) {
                var modalInstance = $modal.open({
                        animation: true,
                        templateUrl: '../input.html',
                        controller: 'Input',
                        size: 'md',
                        scope:$scope
                        });
                modalInstance.result.then(
                        function () {next ($scope.input.message);}, 
                        function () {}
                        );
                };
        $scope.inviting = function (next) {
                var modalInstance = $modal.open({
                        animation: true,
                        templateUrl: '../invite.html',
                        controller: 'Input',
                        size: 'md',
                        scope:$scope
                        });
                modalInstance.result.then(
                        function () {next ($scope.input.mailremainder);}, 
                        function () {}
                        );
                };
        $scope.loggingout = function (next) {
                var modalInstance = $modal.open({
                        animation: true,
                        templateUrl: '../logout.html',
                        controller: 'Input',
                        size: 'md',
                        scope:$scope
                        });
                modalInstance.result.then(
                        function () {next ();}, 
                        function () {}
                        );
                };
        $scope.closing = function (next) {
                var modalInstance = $modal.open({
                        animation: true,
                        templateUrl: '../close.html',
                        controller: 'Input',
                        size: 'md',
                        scope:$scope
                        });
                modalInstance.result.then(
                        function () {next ();}, 
                        function () {}
                        );
                };
        $scope.opening = function (next) {
                var modalInstance = $modal.open({
                        animation: true,
                        templateUrl: '../open.html',
                        controller: 'Input',
                        size: 'md',
                        scope:$scope
                        });
                modalInstance.result.then(
                        function () {next ();}, 
                        function () {}
                        );
                };

        $scope.deleting = function (next) {
                var modalInstance = $modal.open({
                        animation: true,
                        templateUrl: '../delete.html',
                        controller: 'Input',
                        size: 'md',
                        scope:$scope
                        });
                modalInstance.result.then(
                        function () {next ();}, 
                        function () {}
                        );
                };
        $scope.actions= function(x) {
                var as=Array()
                if(x.canVote)
                        as.push({action:function (){$scope.voteUp(x.id)},
                            glyphicon:"glyphicon glyphicon-thumbs-up",text:"Apprezza"});
                if(x.canVote)
                        as.push({action:function (){$scope.voteDown(x.id)},
                            glyphicon:"glyphicon glyphicon-thumbs-down",text:"Disprezza"});
                if(x.canIntervein)
                        as.push({action:function(){$scope.respond(x.id)},
                        glyphicon:"glyphicon glyphicon-comment",text:"Intervieni"});
                if(x.canRespond)
                        as.push({action:function(){$scope.respond(x.id)},
                        glyphicon:"glyphicon glyphicon-envelope",text:"Rispondi"});
                if(x.canClose)
                        as.push({action:function(){$scope.closeConv(x.id)},
                                glyphicon:"glyphicon glyphicon-check",text:"Chiudi"});
                if(x.canOpen)
                        as.push({action:function(){$scope.openMessage(x.id)},
                            glyphicon:"glyphicon glyphicon-share",text:"Apri"});
                if(x.canRetract){
                        as.push({action:function(){$scope.retractMessage(x)},
                          glyphicon:"glyphicon glyphicon-trash",text:"Cancella"});
                        as.push({action:function(){$scope.correctMessage(x)},
                          glyphicon:"glyphicon glyphicon-pencil",text:"Correggi"});
                        }
                return as;
                }
         $scope.getConversation = function(id) {
                $scope.getPersonal();
                $scope.getDetti();
                $scope.getOpens();
                $scope.getRoots();
                $scope.lastConversation.push(id);
                $scope.notgetting=false;
                $http.get("../api/Conversation/" + $scope.userkey + "/" + id).success (function(messages) {
			if(!$scope.stopRefresh){
				$scope.conversation=messages.result;
				for(var i=0;i< $scope.conversation.length;i++){
					$scope.conversation[i].actions=$scope.actions($scope.conversation[i]);
					$scope.conversation[i].roll=$scope.conversation[i].alter.indexOf($scope.conversation[i].id);
					}
				$scope.messageid=id;
				$log.log("refreshed" + id);
				}
                        $scope.notgetting=true;
                        $timeout(function (){ 
				$('.dropper').on('show.bs.dropdown', function () {
					$scope.stopRefresh=true;
					});
				$('.dropper').on('hidden.bs.dropdown', function () {
					$scope.stopRefresh=false;
					});
				});
                        });
                }
	$scope.input.getConversation=function(id) {
		$scope.getConversation(id);
		};
        $scope.rollHome=function(x){
	    x.roll=0;
            $scope.getConversation(x.alter[x.roll]);
            }
        $scope.rollLeft=function(x){
            if(x.roll>0)
              x.roll -= 1;
	    else x.roll=x.alter.length - 1;
            $scope.getConversation(x.alter[x.roll]);
            }
        $scope.rollRight=function(x){
            if(x.roll<x.alter.length-1)
              x.roll += 1;
	    else x.roll=0;
            $scope.getConversation(x.alter[x.roll]);
             
            }
        $scope.invite=function (){
                $scope.inviting(function(){
                  $http.post("../api/Invite/"+$scope.userkey,$scope.input.mailremainder).success(
                        function () {$scope.input.mailremainder=null;});
                });

                }

        $scope.logout=function (){
                $scope.loggingout(function(){
                  $http.put("../api/Logout/"+$scope.userkey).success(
                          function () {location.reload();});
                          });
        }

        $scope.reminds=function (){
                  $http.post("api/Reminds",$scope.input.mailremainder).success(
                          function () {location.reload();});
        }
        $scope.closeConv=function(id){
                $scope.closing(function(){
                  $http.put("../api/Close/"+$scope.userkey +"/" + id).success(
                        function () {$scope.getConversation(id)});
                });
                }

        $scope.openMessage=function(id){
                $scope.opening(function(){
                  $http.put("../api/Open/"+$scope.userkey +"/" + id).success(
                          function () {$scope.getConversation(id)});
                  });
                }
        $scope.respond=function(id){
                $scope.open(function (){
                        $http.post("../api/New/"+$scope.userkey + "/Attach/" + id,$scope.input.message).success(
                                function (rs) {
                                        if(rs.events[0].newmessage)
                                            $scope.getConversation(rs.events[0].newmessage);
                                        else $scope.getConversation(0);
                                            $scope.input.message=null;
                                          
                                });
                        })
                };
        $scope.moot=function(){
                $scope.open(function (){
                        $http.post("../api/New/"+$scope.userkey + "/DontAttach",$scope.input.message).success(
                                function (rs) {
                                        if(rs.events[0].newmessage)
                                            $scope.getConversation(rs.events[0].newmessage);
                                        else $scope.getConversation(0);
                                            $scope.input.message=null;
                                          
                                });
                        })
                };
       $scope.correctMessage=function(x){
                $scope.input.message=x.text;
                $scope.open(function (){
                        $http.post("../api/New/"+$scope.userkey + "/Correct/" + x.id,$scope.input.message).success(
                                function () {
                                        $scope.getConversation(x.id);
                                        $scope.input.message=null;
                                });
                        })
                };

        $scope.retractMessage = function (x){
                $scope.deleting(function (){
                    $http.put("../api/Retract/"+$scope.userkey + "/" + x.id).success(
                    function () {
                        if(x.parent) {
                            $scope.getConversation(x.parent);
                            }
                        else 
                            $scope.getConversation(0);
                            
                        });
                    });
                }
             
        //voting stuff
        $scope.voteUp=function(id){
                $http.put("../api/Vote/"+$scope.userkey + "/" +  id  + "/True").success(function () {$scope.getConversation(id)});}

        $scope.voteDown=function(id){
                $http.put("../api/Vote/"+$scope.userkey + "/" + id + "/False").success(function () {$scope.getConversation(id)});};
        $timeout(function (){
                $scope.getConversation($scope.messageid)
            });
	$scope.redate=function(input){return input.replace(":","").replace(":","").replace(" ","T").concat("Z")};
        $interval(function (){
            if($scope.notgetting)
                $scope.getConversation($scope.messageid)}
            ,10000);
        });
}


/*

        // messaging stuff
        $scope.correct=function(mid){
        if($scope.message){
                $http.post("../api/New/"+$scope.userkey + "/Correct/" + mid,$scope.message).success(
                        function () {$scope.getConversation(mid)});
                $scope.message=null;
                }
        };

        $scope.open = function(){
        if($scope.message){
                $http.post("../api/New/"+$scope.userkey + "/DontAttach",$scope.message).success(
                        function () {$scope.getConversation(0)});
                $scope.message=null;
                }
        };

        $scope.leave = function (){
        $http.put("../api/Leave/"+$scope.userkey + "/Accept/" + mid).success(
                function () {$scope.getConversation(mid)});
        }

        $scope.reminds=function (){
        $http.post("api/Reminds",$scope.mailremainder).success(
                function () {$scope.mailremainder=null});
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


}
*/

