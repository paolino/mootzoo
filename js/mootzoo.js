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

app.controller('conversations', function($scope,$timeout,$modal,$log,$http) {
        $scope.message="ciao";        
        $scope.input= {};
        $scope.messageid=0;
        $scope.setUserkey=function(u) {
                $scope.userkey=u;
                $scope.getConversation($scope.messageid);
                }
        $scope.getLogins=function(){$http.get("../api/Logins").success(function(xs){
                        $scope.logins=xs.result;
                        $scope.setUserkey($scope.logins[0]);
                        });
                }
        $scope.getLogins();
        $scope.getRoots=function(){$http.get("../api/Roots/" + $scope.userkey).success(function(xs){
                        $scope.roots=xs.result;
                        });
                }
        $timeout($scope.getRoots);
        $scope.getPersonal=function(){$http.get("../api/Personal/" + $scope.userkey).success(function(xs){
                        $scope.personali=xs.result;
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
        $scope.actions= function(x) {
                var as=Array()
                if(x.canVote)
                        as.push({action:function (){$scope.voteUp(x.id)},
                            glyphicon:"glyphicon glyphicon-thumbs-up",tooltip:"Apprezza"});
                if(x.canVote)
                        as.push({action:function (){$scope.voteDown(x.id)},
                            glyphicon:"glyphicon glyphicon-thumbs-down",tooltip:"Disprezza"});
                if(x.canIntervein)
                        as.push({action:function(){$scope.respond(x.id)},
                        glyphicon:"glyphicon glyphicon-comment",tooltip:"Intervieni"});
                if(x.canRespond)
                        as.push({action:function(){$scope.respond(x.id)},
                        glyphicon:"glyphicon glyphicon-envelope",tooltip:"Rispondi"});
                if(x.canClose)
                        as.push({action:function(){$scope.closeConv(x.id)},
                                glyphicon:"glyphicon glyphicon-check",tooltip:"Chiudi"});
                if(x.canOpen)
                        as.push({action:function(){$scope.openMessage(x.id)},
                            glyphicon:"glyphicon glyphicon-share",tooltip:"Apri"});
                if(x.canRetract){
                        as.push({action:function(){$scope.retractMessage(x)},
                          glyphicon:"glyphicon glyphicon-trash",tooltip:"Rinuncia"});
                        as.push({action:function(){$scope.correctMessage(x)},
                          glyphicon:"glyphicon glyphicon-pencil",tooltip:"Correggi"});
                        }
                return as;
                }
            $scope.getConversation = function(id) {
                $scope.getRoots();
                $scope.getPersonal();
                $scope.lastConversation.push(id);
                $http.get("../api/Conversation/" + $scope.userkey + "/" + id).success (function(messages) {
                        $scope.conversation=messages.result;
                        for(var i=0;i< $scope.conversation.length;i++){
                                $scope.conversation[i].actions=$scope.actions($scope.conversation[i]);
                                $scope.conversation[i].roll=$scope.conversation[i].alter.indexOf($scope.conversation[i].id);
                                }
                        
                        $scope.messageid=id;
                        
                        });
                }
        $scope.rollLeft=function(x){
            if(x.roll>0){
              x.roll -= 1;
              $scope.getConversation(x.alter[x.roll]);
              }
            }
        $scope.rollLeft0=function(x){
            if(x.roll>0){
              x.roll = 0;
              $scope.getConversation(x.alter[x.roll]);
              }
            }
        $scope.canRollLeft=function(x){
            return(x.roll > 0);
            }
        $scope.rollRight=function(x){
            if(x.roll<x.alter.length-1){
              x.roll += 1;
              $scope.getConversation(x.alter[x.roll]);
              }
            }
        $scope.canRollRight=function(x){
            return(x.roll < x.alter.length - 1);
            }
        $scope.invite=function (){
                $http.post("../api/Invite/"+$scope.userkey,$scope.mailremainder).success(
                        function () {$scope.getConversation(0);$scope.mailremainder=null;$scope.getLogins();});
                }


        $scope.closeConv=function(id){
                $http.put("../api/Close/"+$scope.userkey +"/" + id).success(
                        function () {$scope.getConversation(id)});
                }

        $scope.openMessage=function(id){
                $http.put("../api/Open/"+$scope.userkey +"/" + id).success(
                        function () {$scope.getConversation(id)});
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
                $http.put("../api/Retract/"+$scope.userkey + "/" + x.id).success(
                function () {
                    if(x.parent) {
                        $scope.getConversation(x.parent);
                        }
                    else 
                        $scope.getConversation(0);
                        
                    });
                }
             
        //voting stuff
        $scope.voteUp=function(id){
                $http.put("../api/Vote/"+$scope.userkey + "/" +  id  + "/True").success(function () {$scope.getConversation(id)});}

        $scope.voteDown=function(id){
                $http.put("../api/Vote/"+$scope.userkey + "/" + id + "/False").success(function () {$scope.getConversation(id)});};

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


}
*/

