#include <stdio.h>
#include <string.h>

typedef enum {
    Closed,
    Open,
    Locked,
    } State;

const char* state_names[] = {"Closed", "Open", "Locked", };

State global_current_state = Closed;

State state_machine_step(State current_state, const char* fired_event) {
    switch (current_state) {
    case Closed:
        if (strcmp(fired_event, "Push") == 0 && ((3 < 4))) return Closed;
        if (strcmp(fired_event, "lock") == 0) return Locked;
        return current_state;
    case Open:
        if (strcmp(fired_event, "pull") == 0) return Closed;
        return current_state;
    case Locked:
        if (strcmp(fired_event, "unlock") == 0) return Closed;
        return current_state;
    default:
return current_state;
    }
}

#define STATE_MACHINE_RUNNING 1

int main(void) {
    while (STATE_MACHINE_RUNNING) {
        printf("\nCurrent state: %s\n", state_names[global_current_state]);
        printf("Enter event: ");
        char entered_event[256];
        scanf("%255s", entered_event);
        global_current_state = state_machine_step(global_current_state, entered_event);
    }
    return 0;
}
