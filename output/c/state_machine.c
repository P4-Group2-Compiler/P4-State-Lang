#include "state_machine.h"
#include <string.h>


bool state_machine_step(StateMachine* state_machine, Event fired_event) {

    for (int i = 0; i < state_machine->transition_count; i++) {
        Transition current_transition = state_machine->transitions[i];

        #define NAMES_MATCH 0 // (strcmp() returns 0 if the strings are equal)
        if (strcmp(current_transition.src_state.name, state_machine->current_state.name) == NAMES_MATCH
            && strcmp(current_transition.event.name, fired_event.name) == NAMES_MATCH) {

            state_machine->current_state = current_transition.dst_state;

            return true;
        }
    }
    return false;
}