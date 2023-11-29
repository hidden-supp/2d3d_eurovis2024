let measures = new Map();
measures.set("time", "Time");
measures.set("difficulty", "Difficulty");
measures.set("errorcount", "Error Count");
measures.set("errormeasure", "Error Measure");
measures.set("timefirst", "Time First");
measures.set("timeinstreet", "Time in Street State");

let factors = new Map();
factors.set("technique", "Technique");
factors.set("camera", "Camera");
factors.set("order", "Transition Order");

let twocombinations = new Map();
twocombinations.set("technique_camera", "Technique x Camera Animation");
twocombinations.set("technique_order", "Technique x Transition Order");
twocombinations.set("order_camera", "Transition Order x Camera Animation");

let techniques = new Map();
techniques.set("Animated", "Animated");
techniques.set("Dragalong", "Drag-Along");

let cameraOrders = new Map();
cameraOrders.set("RF", "Rotation First");
cameraOrders.set("ZF", "Zoom First");

let renderingOrders = new Map();
renderingOrders.set("CF", "Camera First");
renderingOrders.set("FF", "Feature First");
renderingOrders.set("Coupled", "Coupled");

let result_tabs = document.getElementById("result_tabs");
let line = document.createElement("ul");
result_tabs.appendChild(line);
result_tabs.className += " mb-4";
let results_section = document.getElementById("results_section");

measures.forEach((name, measure) => {
  let tab = document.createElement("li");
  tab.className = "tab mb-4";
  let link = document.createElement("a");
  link.innerHTML = name;
  line.appendChild(tab);
  tab.appendChild(link);
  link.onclick = function () {
    measures.forEach((name, measure) => {
      document.getElementById(measure + "_container").style.display = "none";
    });
    tablinks = document.getElementsByClassName("tab");
    for (i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(" is-active", "");
    }
    document.getElementById(measure + "_container").style.display = "block";
    tab.className += " is-active";
  };

  let container = document.createElement("div");
  container.className = "container";
  container.id = measure + "_container";
  results_section.appendChild(container);

  let titleMeasureTiles = document.createElement("div");
  titleMeasureTiles.className = "tile is-ancestor mt-4";
  container.appendChild(titleMeasureTiles);

  let titleMeasureTile = document.createElement("div");
  titleMeasureTile.className = "tile is-parent";
  titleMeasureTiles.appendChild(titleMeasureTile);

  let measureTitleTile = document.createElement("div");
  measureTitleTile.className = "tile is-child mt-4";
  titleMeasureTile.appendChild(measureTitleTile);
  let titleMeasure = document.createElement("h2");
  titleMeasure.className = "title is-2";
  titleMeasure.innerHTML = name;
  titleMeasure.style.marginTop = "20px";
  measureTitleTile.appendChild(titleMeasure);

  factors.forEach((factorName, factor) => {
    let columnsTitle = document.createElement("div");
    columnsTitle.className = "columns pt-4";
    container.appendChild(columnsTitle);
    let titleColumn = document.createElement("div");
    titleColumn.className = "column";
    columnsTitle.appendChild(titleColumn);
    let title = document.createElement("h2");
    title.className = "title is-4";
    title.innerHTML = factorName;
    titleColumn.appendChild(title);

    let columns = document.createElement("div");
    columns.className = "columns";
    container.appendChild(columns);

    let column = document.createElement("div");
    column.className = "column";
    columns.appendChild(column);
    let graphName =
      "./images/graphs/" + factor + "/" + factor + "_" + measure + ".png";
    let figure = document.createElement("figure");
    figure.className = "image";
    let graph = document.createElement("img");
    graph.src = graphName;
    figure.appendChild(graph);
    column.appendChild(figure);

    let column_diff = document.createElement("div");
    column_diff.className = "column";
    columns.appendChild(column_diff);
    let graphName_diff =
      "./images/graphs/" + factor + "/" + factor + "_" + measure + "_diff.png";
    let figure_diff = document.createElement("figure");
    figure_diff.className = "image";
    let graph_diff = document.createElement("img");
    graph_diff.src = graphName_diff;
    figure_diff.appendChild(graph_diff);
    column_diff.appendChild(figure_diff);

    let columns_table = document.createElement("div");
    columns_table.className = "columns";
    container.appendChild(columns_table);

    let column_table = document.createElement("div");
    column_table.className = "column";
    columns_table.appendChild(column_table);
    let table = createTable(
      "./images/graphs/" + factor + "/" + factor + "_" + measure + ".html"
    );
    column_table.appendChild(table);

    let column_table_diff = document.createElement("div");
    column_table_diff.className = "column";
    columns_table.appendChild(column_table_diff);
    let table_diff = createTable(
      "./images/graphs/" + factor + "/" + factor + "_" + measure + "_diff.html"
    );
    column_table_diff.appendChild(table_diff);
  });

  twocombinations.forEach((name, combination) => {
    let titleTiles = document.createElement("div");
    titleTiles.className = "tile is-ancestor";
    container.appendChild(titleTiles);

    let titleTile = document.createElement("div");
    titleTile.className = "tile is-parent";
    titleTiles.appendChild(titleTile);

    let combinationTitleTile = document.createElement("div");
    combinationTitleTile.className = "tile is-child";
    titleTile.appendChild(combinationTitleTile);
    let title = document.createElement("h2");
    title.className = "title is-4";
    title.innerHTML = name;
    combinationTitleTile.appendChild(title);

    let tiles = document.createElement("div");
    tiles.className = "tile is-ancestor";
    container.appendChild(tiles);

    let verticalTile = document.createElement("div");
    verticalTile.className = "tile is-vertical";
    tiles.appendChild(verticalTile);

    let tile = document.createElement("div");
    tile.className = "tile";
    verticalTile.appendChild(tile);

    let parentTileTitle = document.createElement("div");
    parentTileTitle.className = "tile is-parent is-vertical is-2";
    tile.appendChild(parentTileTitle);

    let parentTile = document.createElement("div");
    parentTile.className = "tile is-parent is-vertical";
    tile.appendChild(parentTile);

    let parentTileTable = document.createElement("div");
    parentTileTable.className = "tile is-parent is-vertical";
    tile.appendChild(parentTileTable);

    let factorValues;
    if (combination.split("_")[0] == "technique") {
      factorValues = techniques;
    } else {
      if (combination.split("_")[0] == "order") {
        factorValues = renderingOrders;
      }
    }
    factorValues.forEach((factorValueName, factorValue) => {
      let childTileTitle = document.createElement("div");
      childTileTitle.className = "tile is-child";
      parentTileTitle.appendChild(childTileTitle);
      let title = document.createElement("h2");
      title.className = "title is-5";
      title.innerHTML = factorValueName;
      childTileTitle.appendChild(title);

      let childTile = document.createElement("div");
      childTile.className = "tile is-child";
      parentTile.appendChild(childTile);
      let graphTechCameraName =
        "./images/graphs/" +
        combination +
        "/" +
        combination.split("_")[1] +
        "_" +
        factorValue +
        "_" +
        measure +
        ".png";
      let figureTechCamera = document.createElement("figure");
      figureTechCamera.className = "image";
      let graphTechCamera = document.createElement("img");
      graphTechCamera.src = graphTechCameraName;
      figureTechCamera.appendChild(graphTechCamera);
      childTile.appendChild(figureTechCamera);

      let childTileTable = document.createElement("div");
      childTileTable.className = "tile is-child";
      parentTileTable.appendChild(childTileTable);
      let tableTechCamera = createTable(
        "./images/graphs/" +
          combination +
          "/" +
          combination.split("_")[1] +
          "_" +
          factorValue +
          "_" +
          measure +
          ".html"
      );
      childTileTable.appendChild(tableTechCamera);
    });

    let comparisonTiles = document.createElement("div");
    comparisonTiles.className = "tile is-ancestor";
    container.appendChild(comparisonTiles);

    let comparisonTile = document.createElement("div");
    comparisonTile.className = "tile is-parent";
    comparisonTiles.appendChild(comparisonTile);

    let comparisonTitleChild = document.createElement("div");
    comparisonTitleChild.className = "tile is-child";
    comparisonTile.appendChild(comparisonTitleChild);
    let tableComparison = createTable(
      "./images/graphs/" +
        combination +
        "/comparison/" +
        measure +
        "/" +
        combination +
        "_comparisons_" +
        measure +
        ".html"
    );
    comparisonTitleChild.appendChild(tableComparison);
  });

  let threecombinations = "technique_order_camera";
  let threecombinationsvalue =
    "Technique x Transition Order x Camera Animation";
  let threecombinationsTiles = document.createElement("div");
  threecombinationsTiles.className = "tile is-ancestor";
  container.appendChild(threecombinationsTiles);

  let titleTiles3 = document.createElement("div");
  titleTiles3.className = "tile is-ancestor";
  container.appendChild(titleTiles3);

  let titleTile3 = document.createElement("div");
  titleTile3.className = "tile is-parent";
  titleTiles3.appendChild(titleTile3);

  let combinationTitleTile3 = document.createElement("div");
  combinationTitleTile3.className = "tile is-child";
  titleTile3.appendChild(combinationTitleTile3);
  let title3 = document.createElement("h2");
  title3.className = "title is-4";
  title3.innerHTML = threecombinationsvalue;
  combinationTitleTile3.appendChild(title3);

  let tiles3 = document.createElement("div");
  tiles3.className = "tile is-ancestor";
  container.appendChild(tiles3);

  let verticalTile3 = document.createElement("div");
  verticalTile3.className = "tile is-vertical";
  tiles3.appendChild(verticalTile3);

  let tile3 = document.createElement("div");
  tile3.className = "tile";
  verticalTile3.appendChild(tile3);

  let parentTileTitle3 = document.createElement("div");
  parentTileTitle3.className = "tile is-parent is-vertical is-2";
  tile3.appendChild(parentTileTitle3);

  let parentTileSubTitle3 = document.createElement("div");
  parentTileSubTitle3.className = "tile is-parent is-vertical is-2";
  tile3.appendChild(parentTileSubTitle3);

  let parentTile3 = document.createElement("div");
  parentTile3.className = "tile is-parent is-vertical";
  tile3.appendChild(parentTile3);

  let parentTileTable3 = document.createElement("div");
  parentTileTable3.className = "tile is-parent is-vertical";
  tile3.appendChild(parentTileTable3);

  techniques.forEach((techniqueName, technique) => {
    let childTileTitle3 = document.createElement("div");
    childTileTitle3.className = "tile is-child";
    parentTileTitle3.appendChild(childTileTitle3);
    let title3 = document.createElement("h2");
    title3.className = "title is-5";
    title3.innerHTML = techniqueName;
    childTileTitle3.appendChild(title3);

    renderingOrders.forEach((renderingOrderName, renderingOrder) => {
      let childTileSubTitle3 = document.createElement("div");
      childTileSubTitle3.className = "tile is-child";
      parentTileSubTitle3.appendChild(childTileSubTitle3);
      let title3 = document.createElement("h2");
      title3.className = "title is-5";
      title3.innerHTML = renderingOrderName;
      childTileSubTitle3.appendChild(title3);

      let childTile3 = document.createElement("div");
      childTile3.className = "tile is-child";
      parentTile3.appendChild(childTile3);
      let graphTechOrderCameraName =
        "./images/graphs/" +
        threecombinations +
        "/" +
        measure +
        "/" +
        technique +
        "_" +
        renderingOrder +
        "_" +
        "camera" +
        "_" +
        measure +
        ".png";
      let figureTechOrderCamera = document.createElement("figure");
      figureTechOrderCamera.className = "image";
      let graphTechOrderCamera = document.createElement("img");
      graphTechOrderCamera.src = graphTechOrderCameraName;
      figureTechOrderCamera.appendChild(graphTechOrderCamera);
      childTile3.appendChild(figureTechOrderCamera);

      let childTileTable3 = document.createElement("div");
      childTileTable3.className = "tile is-child";
      parentTileTable3.appendChild(childTileTable3);
      let tableTechCamera = createTable(
        "./images/graphs/" +
          threecombinations +
          "/" +
          measure +
          "/" +
          technique +
          "_" +
          renderingOrder +
          "_" +
          "camera" +
          "_" +
          measure +
          ".html"
      );
      childTileTable3.appendChild(tableTechCamera);
    });
  });
  let comparisonTiles3 = document.createElement("div");
  comparisonTiles3.className = "tile is-ancestor";
  container.appendChild(comparisonTiles3);

  let comparisonTile3 = document.createElement("div");
  comparisonTile3.className = "tile is-parent";
  comparisonTiles3.appendChild(comparisonTile3);

  let comparisonTileChild = document.createElement("div");
  comparisonTileChild.className = "tile is-child";
  comparisonTile3.appendChild(comparisonTileChild);
  let tableComparison = createTable(
    "./images/graphs/" +
      threecombinations +
      "/" +
      measure +
      "/comparisons/" +
      threecombinations +
      "_comparisons_" +
      measure +
      ".html"
  );
  comparisonTileChild.appendChild(tableComparison);
  container.style.display = "none";
});

function createTable(path) {
  let tableElement = document.createElement("table");

  tableElement.className = "table is-narrow is-fullwidth";

  fetch(path)
    .then((res) => {
      if (!res.ok) {
        return Promise.reject(res);
      }
      return res.text();
    })
    .then((html) => {
      tableElement.innerHTML = html;
      highlightEvidence(tableElement);
    });

  return tableElement;
}

function highlightEvidence(tableElement) {
  //let tds = tableElement.getElementsByTagName("td");

  let trs = tableElement.getElementsByTagName("tr");
  console.log("table*********", trs.length);
  for (let i = 0; i < trs.length; i++) {
    console.log("iiiii", i);
    let ths = trs[i].getElementsByTagName("th");
    if (ths.length > 5) {
      ths[5].style.display = "none";
      ths[6].style.display = "none";
      ths[7].style.display = "none";
    }
    let tds = trs[i].getElementsByTagName("td");
    if (tds.length > 0) {
      if (tds[1].innerHTML.includes("-") && tds[1].innerHTML != "Drag-along") {
        console.log("varibale", tds[1]);
        if (
          (parseFloat(tds[3].innerHTML) <= 0 &&
            parseFloat(tds[4].innerHTML) <= 0) ||
          (parseFloat(tds[3].innerHTML) >= 0 &&
            parseFloat(tds[4].innerHTML) >= 0)
        ) {
          tds[1].style.backgroundColor = "#F5F5F5";
        }
      }
    }
    if (tds.length > 5) {
      tds[5].style.display = "none";
      tds[6].style.display = "none";
      tds[7].style.display = "none";
    }
  }
}
