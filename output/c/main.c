#include <stdio.h> // Til at læse terminal input og output

#include "state_machine.h"

/*
En simpel alarm clock state machine — skrevet i vores syntaks — brugt som eksempel:
    - (Den samme som der er i "alarm_clock_example.txt")
------------------------------------------------------------------------------------------

    StateMachine M {

        Start State IDLE {
            ON ALARM_BTN_PRESS GO ALARM_ARMED;
        }

        State ALARM_ARMED {
            ON ALARM_BTN_PRESS GO IDLE;
            ON ALARM_TIMER_EXPIRED GO ALARM_RINGING;
        }

        State ALARM_RINGING {
            ON ALARM_BTN_PRESS GO IDLE;
            ON SNOOZE_BTN_PRESS GO SNOOZE;
        }

        State SNOOZE {
            ON SNOOZE_TIME_EXPIRED GO ALARM_RINGING;
            ON ALARM_BTN_PRESS GO IDLE;
        }
            
    }

--------------------------------------------------------------------------------
*/

typedef enum STATE_IDENTIFIERS {
    IDLE,
    ALARM_ARMED,
    ALARM_RINGING,
    SNOOZE,

    STATE_ID_COUNT,
} STATE_IDENTIFIERS;

// ---------------------------------------------------------------------------------------
typedef enum EVENT_IDENTIFIERS {
    ALARM_BTN_PRESS,
    ALARM_TIMER_EXPIRED,
    SNOOZE_BTN_PRESS,
    SNOOZE_TIME_EXPIRED,

    EVENT_ID_COUNT,
} EVENT_IDENTIFIERS;

// =======================================================================================

int main(void) {

    StateMachine M  = {0};
    M.state_count   = STATE_ID_COUNT;
    M.start_state   = IDLE;
    M.current_state = M.start_state;

    // IDLE
    M.states[IDLE] = (State){
        .identifier       = IDLE,
        .is_start         = true,
        .transition_count = 1,
        .transitions =
            {
                {ALARM_BTN_PRESS, ALARM_ARMED},
            },
    };

    // ALARM_ARMED
    M.states[ALARM_ARMED] = (State){
        .identifier       = ALARM_ARMED,
        .is_start         = false,
        .transition_count = 2,
        .transitions =
            {
                {ALARM_BTN_PRESS, IDLE},
                {ALARM_TIMER_EXPIRED, ALARM_RINGING},
            },
    };

    // ALARM_RINGING
    M.states[ALARM_RINGING] = (State){
        .identifier       = ALARM_RINGING,
        .is_start         = false,
        .transition_count = 2,
        .transitions =
            {
                {ALARM_BTN_PRESS, IDLE},
                {SNOOZE_BTN_PRESS, SNOOZE},
            },
    };

    // SNOOZE
    M.states[SNOOZE] = (State){
        .identifier       = SNOOZE,
        .is_start         = false,
        .transition_count = 2,
        .transitions =
            {
                {SNOOZE_TIME_EXPIRED, ALARM_RINGING},
                {ALARM_BTN_PRESS, IDLE},
            },
    };

// -------------------------------------------------------------
#define STATE_MACHINE_RUNNING     1
#define STATE_MACHINE_NOT_RUNNING 0

    while (STATE_MACHINE_RUNNING) {

        printf("\nCurrent state: %d\n", M.current_state);
        printf("Enter Event: ");

        EventID event_input;
        scanf("%d", &event_input);

        switch (event_input) {
        case ALARM_BTN_PRESS:
            state_machine_step(&M, event_input);
            break;
        case ALARM_TIMER_EXPIRED:
            state_machine_step(&M, event_input);
            break;
        case SNOOZE_BTN_PRESS:
            state_machine_step(&M, event_input);
            break;
        case SNOOZE_TIME_EXPIRED:
            state_machine_step(&M, event_input);
            break;
        default:
            printf("\nINVALID EVENT!\n");
            break;
        }
    }

    // -------------------------------------------------------------
    return 0;
}

// Terminal command til at køre eksemplet: (Husk at cd til /out/c)
// gcc main.c state_machine.c -o alarm; ./alarm