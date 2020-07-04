module SalsaML

using Salsa

const N_FEATURES = 1
const Sample = NTuple{N_FEATURES, Number}
const learning_rate = 0.0005

@declare_input num_samples(s)::Int
@declare_input sample(s, i::Int)::Sample
@declare_input response(s, i::Int)::Float64

init_lr!(s::Runtime) = set_num_samples!(s, 0)
function new_lr_runtime()
    s = Runtime()
    init_lr!(s)
    return s
end

function insert_training_pair!(s, sample::Sample, response::Float64)
    i = num_samples(s) + 1
    set_num_samples!(s, i)
    set_sample!(s, i, sample)
    set_response!(s, i, response)
end

function predict(s::Runtime, x::Sample)::Float64
    sum(x .* lr_learned_weights(s))
end

# Derived function with no inputs (essentially a constant)
@derived init_weights(s::Runtime) = Tuple(rand(Float64, N_FEATURES))

@derived function lr_learned_weights(s::Runtime)::NTuple{N_FEATURES,Float64}
    if num_samples(s) == 0
        error("Cannot learn a linear regression with no samples.")
    end
    weights = lr_learned_weights_unrolled(s, 0)
    for i in 1:100  # Stop after 100 iterations
        new_weights = lr_learned_weights_unrolled(s, i)
        if abs(sum(new_weights .- weights)) < 0.0001
            @info "stopped after $i iterations"
            return weights
        end
        weights = new_weights
    end
    @info "timed out in learn iterations"
    return weights
end
@derived function lr_learned_weights_unrolled(s::Runtime, iteration::Int)

    if iteration == 0
        return init_weights(s)
    else
        n = num_samples(s)
        weights = lr_learned_weights_unrolled(s, iteration-1)
        δweights_δcost = lr_δweights_δmse(s, iteration-1) .* learning_rate
        weights = weights .- δweights_δcost
        return weights
    end
end

@derived function lr_δweights_δmse(s::Runtime, iteration::Int)
    n = num_samples(s)
    (-2/n) * sum(
        sum(sample(s, i) .* (response(s, i) - lr_predicted_unrolled(s, i, iteration)))
        for i in 1:n
    )
end
@derived function lr_predicted_unrolled(s::Runtime, i::Int, iteration::Int)
    sum(sample(s, i) .* lr_learned_weights_unrolled(s, iteration))
end


# For logging purposes
@derived function lr_mse(s::Runtime)
    n = num_samples(s)
    1/n * sum(
        (response(s, i) - lr_predicted(s, i)) ^ 2
        for i in 1:num_samples(s)
    )
end

end
