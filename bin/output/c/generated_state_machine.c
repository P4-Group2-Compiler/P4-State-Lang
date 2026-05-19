#include <stdio.h>
#include <string.h>
#define RED           "\033[31m"
#define ORANGE        "\033[38;5;208m"
#define YELLOW        "\033[93m"
#define GREEN         "\033[92m"
#define BOLD          "\033[1m"
#define UN_BOLD       "\033[22m"
#define TEXT_RESET    "\033[0m"


/*================================================================================================*/
typedef enum {
    STATE_A,
} State;

State global_current_state = STATE_A;

const char* state_names[] = { "A", };


/*================================================================================================*/
void print_guard_block_msg(const char* transition, const char* guard) {
    printf(ORANGE BOLD"| Event \"%s\" was blocked by the guard: %s \n"TEXT_RESET, transition, guard);
}
void print_unrecognized_event_msg(const char* event) {
    printf(RED BOLD"| Event \"%s\" is unrecognized in state (%s)\n"TEXT_RESET, event, state_names[global_current_state]);
}
void pretty_print_transition(State src_state, State dst_state) {
    printf(GREEN BOLD"| (%s) ---> (%s)\n"TEXT_RESET, state_names[src_state], state_names[dst_state]);
}
/*================================================================================================*/
int event_match(const char* input, const char* event) {
    return (strcmp(input, event) == 0);
}
void transition_to(State dst_state) {
    pretty_print_transition(global_current_state, /*--->*/ dst_state);
    global_current_state = dst_state;
}
/*================================================================================================*/
void state_machine_check(const char* input_event) {

    const char* fired_event = input_event;
    int is_first_pass = 1;

    auto_recheck:
    if (is_first_pass != 1) {
        fired_event = "AUTO";
    }
    is_first_pass = 0;

    switch (global_current_state) {
    case STATE_A:

        goto unrecognized_input_fallback;
    default:
        unrecognized_input_fallback:
        if (fired_event != "AUTO") {
            print_unrecognized_event_msg(fired_event);
        }
        break;
    }
}

/*================================================================================================*/
int main(void) {
    #define STATE_MACHINE_RUNNING 1
    state_machine_check("AUTO");
    while (STATE_MACHINE_RUNNING) {
        printf(YELLOW BOLD"\nCurrent state: "TEXT_RESET"("BOLD"%s"UN_BOLD")\n", state_names[global_current_state]);
        printf(YELLOW BOLD"Event: "TEXT_RESET);
        char entered_event[256];
        scanf("%255s", entered_event);
        if (!event_match(entered_event, "AUTO")) {
            state_machine_check(entered_event);
        } else {
            printf(RED BOLD"| \"AUTO\" is a reserved event-keyword -- It is not allowed as manual input\n"TEXT_RESET);
        }
    }
    return 0;
}
