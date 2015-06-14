angular.module('ui.bootstrap.demo', ['ui.bootstrap']);
angular.module('ui.bootstrap.demo').controller('ModalDemoCtrl', function ($scope, $modal, $log) {

  $scope.messaggi = [];

  $scope.animationsEnabled = true;

  $scope.open = function (size,index) {

    var modalInstance = $modal.open({
      animation: $scope.animationsEnabled,
      templateUrl: 'myModalContent.html',
      controller: 'ModalInstanceCtrl',
      size: size,
      resolve: {
        items: function () {
          return $scope.messaggi;
        }
      }
    });

    modalInstance.result.then(
      function (testo) {
      $scope.messaggi.splice(index,0,testo);
    }, 
      function () {
      $log.info('Modal dismissed at: ' + new Date());
    });
  };

  $scope.toggleAnimation = function () {
    $scope.animationsEnabled = !$scope.animationsEnabled;
  };

});

// Please note that $modalInstance represents a modal window (instance) dependency.
// It is not the same as the $modal service used above.

angular.module('ui.bootstrap.demo').controller('ModalInstanceCtrl', function ($scope, $modalInstance, items) {

  $scope.items = items;
  $scope.testo= "";

  $scope.ok = function () {
    $modalInstance.close($scope.testo);
  };

  $scope.cancel = function () {
    $modalInstance.dismiss('cancel');
  };
});