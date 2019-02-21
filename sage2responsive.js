var dimension = [0, 0];
$(document).on("shiny:connected", function (e) {

    //document.body.style.zoom = "100%";


    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;

    if (dimension[0] >= 2000) {  //SAGE
        //document.write("<style>.rule1 { ... }</style>");
        //document.body.style.fontSize = "500%";
        document.body.style.zoom = "250%";  //it was 400 so the 25% should become 40%

        nozoom = document.getElementById("nozoom");
        nozoom.style.zoom = "40%";


        map = document.getElementById("map_county");
        //map.style.zoom = "200%"; //messes with the height and hover over markers
        map.style.height = "2600px";

        // LEAFLET MAP TEXT size
        // leafletcont = document.getElementsByClassName("leaflet-container");
        //for (var i = 0; i < leafletcont.length; i++) {
        //leafletcont[i].style.fontSize = "60px";
        //}    not working because the javascript is executed first in this case


        boxzoom = document.getElementsByClassName("boxtozoom");
        for (var i = 0; i < boxzoom.length; i++) {
            boxzoom[i].style.zoom = "250%";
        }

        //WHOLE CONTENT PANEL SIZE
        cont = document.getElementsByClassName("content");
        cont[0].style.zoom = "40%";
        cont[0].style.fontSize = "250%";

        //BOX TITLES SIZE
        titles = document.getElementsByClassName("box-title");
        for (var i = 0; i < titles.length; i++) {
            titles[i].style.fontSize = "110%";
        }

        // FIRST SLIDER TEXT SIZE
        slider = document.getElementsByClassName("irs-grid-text");
        for (var i = 0; i < slider.length; i++) {
            slider[i].style.fontSize = "200%";
        }

        // SECOND SLIDER
        nozoomslider = document.getElementById("nozoomslider");
        nozoomslider.style.zoom = "40%";

        //DROPDOWN MENU NO ZOOM for problem in sage color picker
        //  drop = nozoomslider.getElementsByClassName("dropdown-shinyWidgets");
        // for (var i = 0; i < drop.length; i++) {
        //  drop[i].style.fontSize = "40%";
        // }
        //drop = document.getElementById("dropdown-menu-drop427388825");
        //drop.style.zoom = "40%";

        //Range slider no numbers
        tags = nozoomslider.getElementsByClassName("irs-grid-text");
        for (var i = 0; i < tags.length; i++) {
            tags[i].style.fontSize = "80%";
        }
        document.getElementsByClassName("irs-single")[0].style.fontSize = "200%";
        document.getElementsByClassName("irs-min")[0].style.fontSize = "200%";
        document.getElementsByClassName("irs-from")[1].style.fontSize = "200%";
        document.getElementsByClassName("irs-from")[1].style.background = "#428bca00";
        document.getElementsByClassName("irs-to")[1].style.fontSize = "200%";
        document.getElementsByClassName("irs-to")[1].style.background = "#428bca00";

        labels = document.getElementsByClassName("control-label");
        labels[2].style.fontSize = "60px";


    }
    else {
        map = document.getElementById("map_county");
        map.style.height = "800px";

    }

    Shiny.onInputChange("dimension", dimension);
});
$(window).resize(function (e) {
    dimension[0] = window.innerWidth;
    dimension[1] = window.innerHeight;
    Shiny.onInputChange("dimension", dimension);
});