{
var app = angular.module("app",['ui.bootstrap']);
app.controller('Input', function ($scope, $modalInstance) {

          $scope.testo= "";

          $scope.gotMessage = function () {
            $modalInstance.close($scope.testo);
          };

          $scope.cancel = function () {
            $modalInstance.dismiss('cancel');
          };
        });

app.controller('conversations', function($scope,$modal,$log,$http,$timeout) {
        $scope.message="ciao";        
        $scope.open = function () {

                var modalInstance = $modal.open({
                        animation: false,
                        templateUrl: '../input.html',
                        controller: 'Input',
                        size: 'md',
                        });

                modalInstance.result.then(
                        function (testo) {
                                $scope.message=testo;
                                }, 
                        function () {
                                $log.info('Modal dismissed at: ' + new Date());
                                });
                };

        $scope.getConversation = function(mid) {
                $http.get("../api/Conversation/" + $scope.userkey + "/" + mid).success (
                                function(messages) {$scope.messaggi=messages.result;});
                }
        $timeout(function (){$scope.getConversation(9);});
        });

}

/*
        $scope.messaggi = null;


                        
        //voting stuff
        $scope.voteUp=function(mid){
                $http.put("../api/Vote/"+$scope.userkey + "/" +  mid  + "/True").success(function () {$scope.getConversation(mid)});}

        $scope.voteDown=function(i){
        $http.put("../api/Vote/"+$scope.userkey + "/" + mid + "/False").success(
                function () {$scope.getConversation(mid)});
        };

        // messaging stuff
        $scope.correct=function(mid){
        if($scope.message){
                $http.post("../api/New/"+$scope.userkey + "/Correct/" + mid,$scope.message).success(
                        function () {$scope.getConversation(mid)});
                $scope.message=null;
                }
        };

        $scope.respond=function(mid){
        if($scope.message){
                $http.post("../api/New/"+$scope.userkey + "/Attach/" + mid,$scope.message).success(
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
        $scope.retract = function (mid){
        $http.put("../api/Retract/"+$scope.userkey + "/" + mid).success(
                function () {$scope.getConversation(mid.parent)});
        }

        $scope.leave = function (){
        $http.put("../api/Leave/"+$scope.userkey + "/Accept/" + mid).success(
                function () {$scope.getConversation(mid)});
        }

        $scope.invite=function (){
        $http.post("../api/Invite/"+$scope.userkey,$scope.mailremainder).success(
                function () {$scope.getConversation(0);$scope.mailremainder=null;});
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

