{
var app = angular.module("app",['ui.bootstrap','xeditable']);
app.run(function(editableOptions){editableOptions.theme='bs3'});
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
  $scope.following = Array();   
  $scope.input= {};
  $scope.messageid=0;
  $scope.getting=0;
  $scope.stopRefresh=false;

	// aux
	$scope.redate=function(input){return input.replace(":","").replace(":","").replace(" ","T").concat("Z")};
  $scope.color= function(x) {
      if (x.canClose) return {"background-color":"#aaf"}
      if (x.canRespond) return {"background-color":"#afa"};
      if (x.canOpen) return {"background-color":"#aff"};
      return {"background-color":"#ccc"};
      }
  $scope.margin=function(i,j){return {"margin-left":i*j,"margin-top":i*j}}
	// parametric modal. file is the template without extension, next is a function for positive
  $scope.modalopen = function (file,next) {
    var modalInstance = $modal.open({
            animation: true,
            templateUrl: '../' + file + ".html",
            controller: 'Input',
            size: 'md',
            scope:$scope
            });
    modalInstance.result.then(next);
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
    $scope.contains=function(a, obj) {
        for (var i = 0; i < a.length; i++) {
            if (a[i] === obj) {
                return true;
            }
        }
        return false;
    }

  $scope.getConversation = function(id) {
    $scope.notgetting=false;
    $http.get("../api/Conversation/" + $scope.userkey + "/" + id).success (function(messages) {
      if(!$scope.stopRefresh){
        $scope.conversation=messages.result;
        for(var i=0;i< $scope.conversation.length;i++){
          $scope.conversation[i].actions=$scope.actions($scope.conversation[i]);
          $scope.conversation[i].roll=$scope.conversation[i].alter.indexOf($scope.conversation[i].id);
          }
        $scope.messageid=id;
        }
        $scope.notgetting=true;
        });
    }
  $scope.rollHome=function(x){
    x.roll=0;
    $scope.getConversation(x.alter[x.roll]);
    }
  $scope.rollLeft=function(x){
    if(x.roll>0) x.roll -= 1;
    else x.roll=x.alter.length - 1;
    $scope.getConversation(x.alter[x.roll]);
    }
  $scope.rollRight=function(x){
    if(x.roll<x.alter.length-1) x.roll += 1;
    else x.roll=0;
    $scope.getConversation(x.alter[x.roll]);  
    }
  $scope.invite=function (){
    $scope.modalopen('invite',function(){
      $http.post("../api/Invite/"+$scope.userkey,$scope.input.mailremainder).success(
            function () {$scope.input.mailremainder=null;});
      });
    }
  
  $scope.logout=function (){
    $scope.modalopen('logout',function(){
      $http.put("../api/Logout/"+$scope.userkey).success(
        function () {location.reload();});
        });
    }
  $scope.reminds=function (){
    $http.post("api/Reminds",$scope.input.mailremainder).success(
      function () {location.reload();}
      );
    }
  $scope.closeConv=function(id){
    $scope.modalopen('close',function(){
      $http.put("../api/Close/"+$scope.userkey +"/" + id).success(
        function () {$scope.refresh()});
      });
    }
  $scope.openMessage=function(id){
    $scope.modalopen('open',function(){
      $http.put("../api/Open/"+$scope.userkey +"/" + id).success(
        function () {$scope.refresh()});
      });
    }
  $scope.respond=function(id){
    $scope.modalopen('input',function (){
      $http.post("../api/New/"+$scope.userkey + "/Attach/" + id,$scope.input.message).success(
        function (rs) {
          $scope.refresh(); 
          $scope.input.message=null;
            
        });
      })
    };
  $scope.moot=function(){
      $http.post("../api/New/"+$scope.userkey + "/DontAttach",$scope.input.message).success(
        function (rs) {
          $scope.refresh(); 
          $scope.input.message=null;
        });
    };
  $scope.correctMessage=function(x){
    $scope.input.message=x.text;
    $scope.modalopen('input',function (){
      $http.post("../api/New/"+$scope.userkey + "/Correct/" + x.id,$scope.input.message).success(
        function () {
           $scope.refresh(); 
          $scope.input.message=null;
        });
      })
    };
  $scope.updateLabel=function(x,l1,l){
      $http.post("../api/UpdateLabel/"+$scope.userkey + "/" + x.id + "/" + l1,l).success(
        function () {
           $scope.refresh(); 
        });
      return true;
    }
  $scope.removeLabel=function(l,x) {
      $http.post("../api/RemoveLabel/"+$scope.userkey + "/" + x.id,l).success(
        function () {
           $scope.refresh(); 
        });
      return true;

    }
  $scope.labelingTrue=function(id) {
      $scope.labeling = true;
      $timeout($('#newlabel'+id).focus());
      }
  $scope.retractMessage = function (x){
    $scope.modalopen('delete',function (){
        $http.put("../api/Retract/"+$scope.userkey + "/" + x.id).success(
          function () {$scope.refresh()});
        });
    }
  $scope.setLabelfilter = function(x) {
      $scope.labelfilter=x;
        $http.get("../api/LabelMessages/" + $scope.userkey + "/" + $scope.labelfilter).success (
          function (messages) {$scope.filtered=messages.result});
      }

  $scope.setLabelfilter("****");
  $scope.filterActive = function (x) {
      return $scope.labelfilter == x ? 'active' : ''
      }
  //voting stuff
  $scope.voteUp=function(id){
    $http.put("../api/Vote/"+$scope.userkey + "/" +  id  + "/True").success(function () {$scope.getConversation(id)});}
  $scope.voteDown=function(id){
    $http.put("../api/Vote/"+$scope.userkey + "/" + id + "/False").success(function () {$scope.getConversation(id)});};
  
  $scope.dropperclass=function(x){return }
  $scope.refresh= function (){
    getLabels = function (j) {
      $http.get("../api/MessageLabels/" + $scope.userkey +"/"+ $scope.following[j].id).success (
        function(y) {
          $scope.following[j].labels=y.result;
          }
        )
      }
    getNotifications = function(m,x) {
            x.notifications={newscount:0,branchcount:0};
            $http.get("../api/Notificate/" + $scope.userkey +"/"+ m).success (
              function(y) {
                var x = y.result;
                if(x.followup){
                  }
                }
              )
            }
      
    getMessage = function (m) {
            $http.get("../api/Single/" + $scope.userkey +"/"+ m).success (
              function (y) {
                var x=y.result;
                x.actions=$scope.actions(x);
                getNotifications(m,x);
                x.visibility=true;
                // closure for j and id
                var callbacks = function(j){
                      $timeout(function () {
                          $('.bitem'+$scope.following[j].id).draggable();
                          $('.bitem'+$scope.following[j].id).resizable();
                          });
                      }
                // search for a message and update the object if found
                for(j=0;j<$scope.following.length;j++)
                  if ($scope.following[j].id==x.id){
                    $.extend($scope.following[j],x);
                    getLabels(j);
                    break;
                    }
                if (j>=$scope.following.length){
                    $scope.following.push(x);
                    getLabels(j);
                    callbacks(j);
                    }
                // countdown the receiving event
                $scope.getting--;
                }
              ).error(function () {$scope.getting--})
        };
      
    $http.get("../api/Following/" + $scope.userkey ).success (
      function(messages) {
        var ms=messages.result;
        $scope.getting=ms.length;
        for(j=0;j<$scope.following.length;j++){
          for(i=0;i<ms.length;i++)
            if ($scope.following[j].id==ms[i])break;
          if(i>=ms.length)
            $scope.following[j].visibility=false;
          }
          
        for(var i=0;i<messages.result.length;i++)
            getMessage (ms[i]);
        }
      );
    $http.get("../api/Labels/"+ $scope.userkey).success(
      function(labels) {
        $scope.labels = labels.result;
        }
      );
    }
  $timeout(function (){
    $scope.refresh();
    });
  $interval(function (){if($scope.getting <= 0)$scope.refresh()},10000);
  });
    
}


