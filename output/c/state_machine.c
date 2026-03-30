#include "state_machine.h"

/* IKKE BRUGT: Funktion til at init state_machine:
void init_state_machine(StateMachine* state_machine) {

}
*/

// =======================================================================================
// Funktion til at gå et "step" i state machine'n:
StateID state_machine_step(StateMachine* state_machine, EventID fired_event) {
    
    // Vi tager en pointer til det current state som state machine'n er på:
    State* p_current_state = &state_machine->states[state_machine->current_state];

    // Så looper vi gennem alle transitions der tilhører det current_state og tjekker om...
    // ...nogle af deres events matcher det tilsendte fired_event til funktionen:
    for (int i = 0; i < p_current_state->transition_count; i++) {

        if (p_current_state->transitions[i].event == fired_event) {
            // Hvis der er et match opdaterer vi state machine'ns current state:
            state_machine->current_state = p_current_state->transitions[i].next_state;

            return state_machine->current_state;
        }
    }

    // Hvis der IKKE er et match returnerer vi bare det samme current_state igen:
    return state_machine->current_state;
}