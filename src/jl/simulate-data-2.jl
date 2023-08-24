
## without loop
using DataFrames, Random, Distributions, Plots, StatsBase

# Set seed for reproducibility
Random.seed!(1234)

# Parameters
n_students = 60
n_assessments = 6
n_items = 20
practice_amount = 5.0
group_multiplier = 1.2

# Create student-level data
students = DataFrame(
    StudentID = 1:n_students,
    Group = vcat(fill("Control", div(n_students,2)), fill("Self-Regulation", div(n_students,2))),
    PracticeEffectiveness = [i == "Self-Regulation" ? group_multiplier : 1.0 for i in vcat(fill("Control", div(n_students,2)), fill("Self-Regulation", div(n_students,2)))],
    InitialAbility = rand(Normal(10, 2), n_students)
)

# Define state transition function
function transition_ability(prev_ability, practice_effectiveness)
    return prev_ability + practice_amount * practice_effectiveness + rand(Normal(0, 1))
end

# Generate the state-space model data for each student and each assessment
studentIDs = []
groups = []
assessments = []
scores = []

for student in eachrow(students)
    abilities = reduce((acc, pe) -> vcat(acc, transition_ability(acc[end], pe)), 
                       fill(student.PracticeEffectiveness, n_assessments - 1), 
                       init=[student.InitialAbility])
    
    for (a, ability) in enumerate(abilities)
        push!(assessments, a)
        push!(studentIDs, student.StudentID)
        push!(groups, student.Group)
        
        score_prob = cdf(Normal(15, 3), ability)
        score = sum(rand(Bernoulli(score_prob), n_items))
        push!(scores, score)
    end
end

data = DataFrame(StudentID=studentIDs, Assessment=assessments, Group=groups, Score=scores)

# Plot the data
p = plot(groupby(data, [:Assessment, :Group]), 
    x=:Assessment, 
    y=:Score_mean, 
    line=(:Group, [:solid, :dash]), 
    color=:Group, 
    legend=:topright, 
    ylabel="Score", 
    xlabel="Assessment", 
    title="Score by Assessment and Group"
)
display(p)
