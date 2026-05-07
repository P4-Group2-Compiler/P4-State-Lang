#include <stdio.h>
#include <string.h>

typedef enum {
    Open,
    Closed,
    } State;

const char* state_names[] = {"Open", "Closed", };

State global_current_state = Open;

State state_machine_step(State current_state, const char* fired_event) {
    switch (current_state) {
    case Open:
        if (strcmp(fired_event, "Push") == 0 && ((x < 4))) return Closed;
        if (strcmp(fired_event, "Push") == 0 && (NOT MATCHED)) return Home;
        return current_state;
    case Closed:
        if (strcmp(fired_event, "Push") == 0 && (NOT MATCHED)) return Closed;
        if (strcmp(fired_event, "Pull") == 0 && ((x < y))) return Closed;
        if (strcmp(fired_event, "TEST") == 0) return SOMETHING;
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
