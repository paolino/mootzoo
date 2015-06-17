angular.module('plunker', ['ui.bootstrap']);
var ModalDemoCtrl = function ($scope, $modal, $log) {
  $scope.model = {};
  $scope.newmodel={};
  $scope.open = function () {
    var modalInstance = $modal.open({
      templateUrl: 'input.html',
      controller: InputCtrl,
      scope: $scope
    });
    modalInstance.result.then(function (m) {
                $scope.newmodel=m;
                $scope.model=null;
        });
  };
};

// Please note that $modalInstance represents a modal window (instance) dependency.
// It is not the same as the $modal service used above.

var InputCtrl = function ($scope, $modalInstance) {
  $scope.ok = function () {$modalInstance.close($scope.model);};
  $scope.cancel = function () {$modalInstance.dismiss();};
};