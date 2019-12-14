package uk.co.cemerson.aoc.Day14

import uk.co.cemerson.aoc.AOCDay

typealias IngredientStore = MutableMap<String, Long>
typealias IngredientsRequired = MutableList<Pair<String, Long>>
typealias ReactionsList = Map<Pair<String, Long>, List<Pair<String, Long>>>

class Day14 : AOCDay {
    override fun part1() {
        val oreRequired = findOreRequiredForFuel(1L)

        println("Ore Required: " + oreRequired)
    }

    override fun part2() {
        var guess = 1000000000000L / findOreRequiredForFuel(1L)
        var minBound = 0L
        var maxBound: Long

        if (findOreRequiredForFuel(guess) <= 1000000000000L) {
            minBound = guess
            maxBound = guess + 1000000000000L
        } else {
            maxBound = guess
        }

        while (maxBound - minBound > 1) {
            guess = maxBound - ((maxBound - minBound) / 2)

            if (findOreRequiredForFuel(guess) <= 1000000000000L) {
                minBound = guess
            } else {
                maxBound = guess
            }
        }

        println(minBound.toString() + " fuel requires " + findOreRequiredForFuel(minBound) + " ore")
    }

    private fun findOreRequiredForFuel(fuel: Long): Long {
        val reactions = getReactions()

        var currentIngredientRequirements = mutableListOf(Pair("FUEL", fuel))
        var ingredientStore = mutableMapOf<String, Long>()

        while (currentIngredientRequirements.filter { it.first != "ORE" }.count() > 0) {
            val result = refineIngredients(currentIngredientRequirements, ingredientStore, reactions)

            currentIngredientRequirements = result.first
            ingredientStore = result.second
        }

        return currentIngredientRequirements.filter { it.first == "ORE" }.map { it.second }.first()
    }

    private fun refineIngredients(currentIngredientRequirements: IngredientsRequired, ingredientStore: IngredientStore, reactions: ReactionsList): Pair<IngredientsRequired, IngredientStore> {
        //Pick first non-ore item in the list of requirements
        val ingredientToFetch = currentIngredientRequirements
                .filter { it.first != "ORE" }
                .first()

        val ingredientRequired = ingredientToFetch.first
        var quantityRequired = ingredientToFetch.second

        //Take from ingredient store if we can
        val ingredientStoreListForThisIngredient = ingredientStore.filter { it.key == ingredientRequired }

        if (ingredientStoreListForThisIngredient.count() > 0) {
            val quantityInIngredientStore = ingredientStoreListForThisIngredient.map { it.value }.first()

            if (quantityRequired < quantityInIngredientStore) {
                ingredientStore[ingredientRequired] = quantityInIngredientStore - quantityRequired
                quantityRequired = 0
            } else {
                quantityRequired -= quantityInIngredientStore
                ingredientStore.remove(ingredientRequired)
            }
        } else {
            quantityRequired = ingredientToFetch.second
        }

        //Refine what's left
        val reaction = reactions.filter { it.key.first == ingredientRequired }

        val quantityProduced = reaction.map { it.key.second }.first()
        val requiredIngredients = reaction.map { it.value }.first()

        val timesToPerformReaction = Math.ceil(quantityRequired.toDouble() / quantityProduced.toDouble()).toLong()

        //Add any spare to store
        val remainingIngredientAfterReaction = (quantityProduced * timesToPerformReaction) - quantityRequired

        if (remainingIngredientAfterReaction > 0) {
            if (ingredientStore.filter { it.key == ingredientRequired }.count() == 0) {
                ingredientStore[ingredientRequired] = 0
            }

            ingredientStore[ingredientRequired] = ingredientStore[ingredientRequired]!! + remainingIngredientAfterReaction
        }

        //Add requiredIngredients to current ingredient requirements
        requiredIngredients
                .map { Pair(it.first, it.second * timesToPerformReaction) }
                .forEach {
                    val thisIngredient = it.first
                    val thisQuantity = it.second

                    if (currentIngredientRequirements.filter { it.first == thisIngredient }.count() == 0) {
                        currentIngredientRequirements.add(Pair(thisIngredient, thisQuantity))
                    } else {
                        val currentQuantity = currentIngredientRequirements.filter { it.first == thisIngredient }.map { it.second }.first()
                        currentIngredientRequirements.remove(Pair(thisIngredient, currentQuantity))
                        currentIngredientRequirements.add(Pair(thisIngredient, currentQuantity + thisQuantity))
                    }
                }

        currentIngredientRequirements.remove(ingredientToFetch)

        return Pair(currentIngredientRequirements, ingredientStore)
    }

    private fun getReactions(): ReactionsList {
        val reactions = mutableMapOf<Pair<String, Long>, List<Pair<String, Long>>>()

        readFileLines("Day14/input.txt")
                .forEach {
                    val sides = it.split("=>")
                    val result = sides[1].trim().split(' ')
                    val ingredients = sides[0].trim().split(',')

                    val ingredientsList = mutableListOf<Pair<String, Long>>()

                    ingredients.forEach {
                        val thisIngredient = it.trim().split(' ')
                        ingredientsList.add(Pair(thisIngredient[1], thisIngredient[0].toLong()))
                    }

                    reactions[Pair(result[1], result[0].toLong())] = ingredientsList
                }

        return reactions
    }
}