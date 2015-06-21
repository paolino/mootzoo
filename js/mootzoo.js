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
        $scope.setUserkey=function(u) {
                $scope.userkey=u;
                $scope.getConversation($scope.messageid);
                }
        $scope.getLogins=function(){$http.get("../api/Logins").success(function(xs){
                        $scope.logins=xs.result;
                        });
                }
        $scope.getLogins();
        $scope.getRoots=function(){$http.get("../api/Roots/" + $scope.userkey + "/0").success(function(xs){
                        $scope.roots=xs.result;
                        for(var i=0;i< $scope.roots.length;i++)
                                $scope.roots[i].actions=$scope.actions($scope.roots[i]);
                        });
                }
        $timeout($scope.getRoots);
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
                        as.push({action:function (){$scope.voteUp(x.id)},text:":>",tooltip:"Bel messaggio"});
                if(x.canVote)
                        as.push({action:function (){$scope.voteDown(x.id)},text:":<",tooltip:"Pessimo messaggio"});
                if(x.canIntervein)
                        as.push({action:function(){$scope.respond(x.id)},text:"P",tooltip:"Richiesta di apertura all'autore"});
                if(x.canRespond)
                        as.push({action:function(){$scope.respond(x.id)},text:"R",tooltip:"Messaggio di risposta"});
                if(x.canClose)
                        as.push({action:function(){$scope.closeConv(x.id)},text:"T",tooltip:"Chiudi la conversazione"});
                if(x.canOpen)
                        as.push({action:function(){$scope.openMessage(x.id)},text:"A",tooltip:"Apri la risposta a chiunque"});
                if(x.canRetract)
                        as.push({action:function(){$scope.retractMessage(x)},text:"Z",tooltip:"Ritira il tuo messaggio"});
                return as;
                }
                $scope.getConversation = function(id) {
                $scope.getRoots();
                $scope.lastConversation.push(id);
                $http.get("../api/Conversation/" + $scope.userkey + "/" + id + "/1").success (function(messages) {
                        $scope.conversation=messages.result;
                        for(var i=0;i< $scope.conversation.length;i++){
                                $scope.conversation[i].actions=$scope.actions($scope.conversation[i]);
                                for(var j=0;j< $scope.conversation[i].nodes.length;j++)
                                        $scope.conversation[i].nodes[j].actions=$scope.actions($scope.conversation[i].nodes[j]);
                                }
                        $scope.messageid=messages.result[0].id;
                        
                        });
                }
        $scope.filnodes= function(x,i) {return x.nodes.filter(function(z){
              if($scope.conversation[i + 1])
            
                return (z.id != $scope.conversation[i + 1].id);
              return true;
              })}
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
                                function () {
                                        $scope.getConversation(id);
                                        $scope.input.message=null;
                                });
                        })
                };
        $scope.moot=function(){
                $scope.open(function (){
                        $http.post("../api/New/"+$scope.userkey + "/DontAttach",$scope.input.message).success(
                                function () {
                                        $scope.getConversation(0);
                                        $scope.input.message=null;
                                });
                        })
                };

        $scope.retractMessage = function (x){
                $http.put("../api/Retract/"+$scope.userkey + "/" + x.id).success(
                function () {
                    if(x.parent) {
                        $scope.getConversation(x.parent);
                        $scope.lastConversation.pop(); 
                        $scope.lastConversation.push(x.parent);
                        }
                    else 
                        $scope.getConversation($scope.lastConversation.pop());
                        
                    });
                }
             
        //voting stuff
        $scope.voteUp=function(id){
                $http.put("../api/Vote/"+$scope.userkey + "/" +  id  + "/True").success(function () {$scope.getConversation(id)});}

        $scope.voteDown=function(id){
                $http.put("../api/Vote/"+$scope.userkey + "/" + id + "/False").success(function () {$scope.getConversation(id)});};

        $timeout(function (){$scope.getConversation(0);});
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

