#include "state_machine.h"
#include <stdio.h>

typedef enum {
    Closed,
    Open,
    Locked,
    
    STATE_COUNT
} StateID;

int main(void) {
    StateMachine Door = {0};
    Door.states[Closed] = (State){.name = "Closed"};
    Door.states[Open] = (State){.name = "Open"};
    Door.states[Locked] = (State){.name = "Locked"};
    Door.state_count = STATE_COUNT;

    Door.start_state = Door.states[Closed];
    Door.current_state = Door.start_state;
    Door.final_states[0] = Door.states[Locked];

    Door.transitions[0] = (Transition){
        .src_state = Door.states[Closed],
        .event     = (Event){.name = "push"},
        .dst_state = Door.states[Open]
    };
    Door.transitions[1] = (Transition){
        .src_state = Door.states[Closed],
        .event     = (Event){.name = "lock"},
        .dst_state = Door.states[Locked]
    };
    Door.transitions[2] = (Transition){
        .src_state = Door.states[Open],
        .event     = (Event){.name = "pull"},
        .dst_state = Door.states[Closed]
    };
    Door.transitions[3] = (Transition){
        .src_state = Door.states[Locked],
        .event     = (Event){.name = "unlock"},
        .dst_state = Door.states[Closed]
    };
    Door.transition_count = 4;
    
#define STATE_MACHINE_RUNNING true

    while (STATE_MACHINE_RUNNING) {
        printf("\nCurrent state: %s\n", Door.current_state.name);
        printf("Enter event: ");
        char entered_event_name[256];
        while (scanf("%s255", &entered_event_name) != true) {
            printf("\nInvalid Input!\n");
            while (getchar() != '\n');
        };
        Event entered_event = (Event){.name = entered_event_name};
        state_machine_step(&Door, entered_event);
    }

    return 0;
}