#ifndef STATE_MACHINE_H
#define STATE_MACHINE_H

#include <stdbool.h>

#define MAX_TRANSITIONS 10
#define MAX_STATES      10

// =======================================================================================
/* (Skabelon/eksempel):
typedef enum STATE_IDENTIFIERS {
    A,
    B,
    C,
} STATE_IDENTIFIERS;
*/
// =======================================================================================
/* (Skabelon/eksempel):
typedef enum EVENT_IDENTIFIERS {
    a,
    b,
    c,
} EVENT_IDENTIFIERS;
*/
// =======================================================================================
typedef int EventID;
typedef int StateID;

// =======================================================================================
typedef struct Transition {
    EventID event;
    StateID next_state;
} Transition;

// =======================================================================================
typedef struct State {
    bool is_start;

    StateID    identifier;
    int        transition_count;
    Transition transitions[MAX_TRANSITIONS];
    //State*   state_pointers[MAX_TRANSITIONS];
} State;

// =======================================================================================
typedef struct StateMachine {
    State   states[MAX_STATES];
    int     state_count;
    StateID start_state;
    StateID current_state;
} StateMachine;

// =======================================================================================
// void init_state_machine(StateMachine* state_machine); (Ikke brugt alligevel)
StateID state_machine_step(StateMachine* state_machine, EventID event);

// =======================================================================================

/*
NOTER:
    - Linked lists???
        https://www.geeksforgeeks.org/c/c-program-to-implement-singly-linked-list/

*/

#endif // STATE_MACHINE_H