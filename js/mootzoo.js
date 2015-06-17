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
        $scope.past=function(){
                if($scope.conversation[0].parent)
                $scope.getConversation($scope.conversation[0].parent);
                }
        $scope.gotox=function(){
                $scope.getConversation($scope.gotoConversation);
                }
        $scope.open = function (next) {
                var modalInstance = $modal.open({
                        animation: false,
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
                        as.push({action:function (){$scope.voteUp(x.id)},text:"Mi piace",tooltip:"Gradimento del messaggio"});
                if(x.canVote)
                        as.push({action:function (){$scope.voteDown(x.id)},text:"Non mi piace",tooltip:"Gradimento del messaggio"});
                if(x.canIntervein)
                        as.push({action:function(){$scope.respond(x.id)},text:"Intervieni",tooltip:"Manda un messaggio all'autore per iniziare una conversazione"});
                if(x.canRespond)
                        as.push({action:function(){$scope.respond(x.id)},text:"Rispondi",tooltip:"Manda un messaggio all'autore in risposta"});
                if(x.canClose)
                        as.push({action:function(){$scope.closeConv(x.id)},text:"Chiudi",tooltip:"Chiudi la conversazione"});
                if(x.canOpen)
                        as.push({action:function(){$scope.openMessage(x.id)},text:"Apri",tooltip:"Apri la risposta al messaggio a chiunque"});
                if(x.canRetract)
                        as.push({action:function(){$scope.retractMessage(x)},text:"Retract",tooltip:"Ritira il tuo messaggio"});
                return as;
                }
        $scope.getConversation = function(id) {
                $http.get("../api/Conversation/" + $scope.userkey + "/" + id).success (function(messages) {
                        $scope.conversation=messages.result;
                        for(var i=0;i< $scope.conversation.length;i++)
                                $scope.conversation[i].actions=$scope.actions($scope.conversation[i]);
                        $scope.messageid=messages.result[0].id;
                        });
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
                function () {$scope.getConversation(x.parent)});
                }
             
        //voting stuff
        $scope.voteUp=function(id){
                $http.put("../api/Vote/"+$scope.userkey + "/" +  id  + "/True").success(function () {$scope.getConversation(id)});}

        $scope.voteDown=function(i){
        $http.put("../api/Vote/"+$scope.userkey + "/" + id + "/False").success(
                function () {$scope.getConversation(id)});
        };
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

