var prizes = [[1, 1, 100], [2, 2, 50], [3, 4, 25]];
var guesses = [65, 70, 78, 65, 72];
var answer = 70;

function awardedPrizes(prizes, guesses, answer) {
    var winning = getWinning(guesses, answer);
    prizes = allPrizes(prizes);
    console.log("Prizes", prizes);
    console.log("Guesses", winning);
    console.log("Sorted", filterWinning(guesses, answer));
    console.log("testing", fixedPrize(100, 3));
    var winningNumbers = filterWinning(guesses, answer);
    var ascList = zip(prizes, winningNumbers);
    var gg = sumDuplicates(ascList);
    
     console.log(ascList);
    
    //return winning;

}

function zip(array1, array2){
    return array1.map( (x,y) =>{
        var occr = count(array2[y], array2);
        return [occr,x,array2[y]];
    });
}

function count(num, array){
    return array.filter((x) => x == num).length; 
}

function getTied(winningNums, num) {
    return winningNums.reduce(function (p, v, i) {
        if (v === num)
            return p.concat(i);
        return p;
    }, []);
}

function allPrizes(prizes) {
    var results = [];
    for (var j = 0; j < prizes.length; j++) {
        if ((prizes[j][0] - prizes[j][1]) === 0) {
            results[prizes[j][0] - 1] = prizes[j][2];
        }
        else {
            for (var k = (prizes[j][0] - 1); k < prizes[j][1]; k++) {
                results[k] = prizes[j][2];
            }
        }
    }
    return results;
}

function fixedPrize(prize, num) {
    return Math.round((prize / num));
}

function filterWinning(guesses, answer) {
    return guesses.map(function (a) { return answer - a; }).sort(function (a, b) { return Math.abs(a) > Math.abs(b) ? 1 : -1; }).sort(function (a, b) { return ((a + "").indexOf("-") > -1 && a < b) ? 1 : -1; });
}
function getWinning(guesses, answer) {
    return guesses.map(function (a) { return answer - a; });
}
console.log("Final", awardedPrizes(prizes, guesses, answer));
