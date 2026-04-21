#ifndef STATE_MACHINE_H
#define STATE_MACHINE_H

#include <stdbool.h>

#define MAX_TRANSITIONS 256
#define MAX_STATES      256

// =======================================================================================
typedef struct State {
    char* name;
} State;

// =======================================================================================
typedef struct Event {
    char* name;
} Event;

// =======================================================================================
typedef struct Transition {
    State src_state;
    Event event;
    State dst_state;
} Transition;

// =======================================================================================
typedef struct StateMachine {
    char*      name;
    State      states[MAX_STATES];
    State      start_state;
    State      final_states[MAX_STATES];
    State      current_state;
    int        state_count;
    Transition transitions[MAX_TRANSITIONS];
    int        transition_count;
} StateMachine;

// =======================================================================================
bool state_machine_step(StateMachine* state_machine, Event event);

// =======================================================================================


#endif // STATE_MACHINE_H