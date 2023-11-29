let techniques = ["Animated", "Dragalong"];

let transitionOrders = ["CF", "Coupled", "FF"];

let cameraTransitions = ["RF", "ZF"];

let allCombinations = [];
for (let technique of techniques) {
  for (let transitionOrder of transitionOrders) {
    for (let cameraTransition of cameraTransitions) {
      allCombinations.push({
        technique: technique,
        transitionOrder: transitionOrder,
        cameraTransition: cameraTransition,
      });
    }
  }
}

console.log(allCombinations);

let meanErrorElement = document.getElementById("mean_error_div");
for (let technique of techniques) {
  let columnsEl = document.createElement("div");
  columnsEl.className = "columns";
  let techniqueEl = document.createElement("div");
  techniqueEl.className = "column";
  let nameTechnique = document.createElement("p");
  nameTechnique.appendChild(technique);
  meanErrorElement.appendChild(columnsEl);
  for (let transitionOrder of transitionOrders) {
    let columnsOrder = document.createElement("div");
  }
}

function createColumns() {
  let columns = document.createElement("div");
}
