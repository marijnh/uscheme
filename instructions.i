// Specification of all instructions. These are included in two
// different ways, once to create the actual enum and once to 
// create a list of those enum values combined with their names.

INSTRUCTION(return)
INSTRUCTION(jump)
INSTRUCTION(jump_if_false)

INSTRUCTION(setup_arg_list)
INSTRUCTION(add_arg)

INSTRUCTION(literal)

INSTRUCTION(call)
INSTRUCTION(tail)
INSTRUCTION(finish_lambda)
INSTRUCTION(make_macro)

INSTRUCTION(grow_env)
INSTRUCTION(deref_env)
INSTRUCTION(deref_ref)
INSTRUCTION(define_ref)
INSTRUCTION(set_env)
INSTRUCTION(set_ref)

INSTRUCTION(as_arguments)
INSTRUCTION(current_continuation)
INSTRUCTION(set_continuation)
