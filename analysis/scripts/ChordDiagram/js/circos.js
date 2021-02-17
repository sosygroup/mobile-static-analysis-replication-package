var width = 550;
var height = 550;

var svg = d3.select("#chart")
        .append("svg")
        .attr("width", width)
        .attr("height", height)
        .append("g")
        .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");

var matrix5x5 = [
    [0, 5, 7, 2, 0],
    [5, 0, 3, 4, 1],
    [7, 3, 0, 6, 3],
    [2, 4, 6, 0, 8],
    [0, 1, 3, 8, 0]
];

var matrix4x4 = [
    [0, 5, 7, 2],
    [10, 10, 10, 10],
    [7, 3, 0, 6],
    [0, 1, 3, 8]
];

var matrix = [
    [232, 29],
    [29, 1]
];

var range5 = ["#33585e", "#F26223"];

var chord = d3.layout.chord()
        .padding(.05)
        .sortSubgroups(d3.descending)
        .matrix(matrix);

var fill = d3.scale.ordinal()
        .domain(d3.range(range5.length))
        .range(range5);


var innerRadius = Math.min(width, height) * .41;
var outerRadius = innerRadius * 1.1;

svg.append("g")
        .selectAll("path")
        .data(chord.groups)
        .enter().append("path")
        .style("fill", function(d) {
            return fill(d.index);
        })
        .style("stroke", function(d) {
            return fill(d.index);
        })
        .attr("d", d3.svg.arc().innerRadius(innerRadius).outerRadius(outerRadius))
        .on("mouseover", fade(.1))
        .on("mouseout", fade(1));

svg.append("g")
        .attr("class", "chord")
        .selectAll("path")
        .data(chord.chords)
        .enter().append("path")
        .style("fill", function(d) {
            return fill(d.target.index);
        })
        .attr("d", d3.svg.chord().radius(innerRadius))
        .style("opacity", 1);

// Add a text label.
var groupText = svg.append("text")
.attr("x", 15)
.attr("dy", 15);
 
groupText.append("textPath")
.text(function(d, i) { return "test" });

function fade(opacity) {
    return function(g, i) {
        svg.selectAll("g.chord path")
                .filter(function(d) {
                    return d.source.index != i && d.target.index != i;
                })
                .transition()
                .style("opacity", opacity);
    };
}
