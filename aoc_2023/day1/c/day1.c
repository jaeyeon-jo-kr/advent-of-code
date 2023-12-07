#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>


int first_digit(const char* line){
    int i = 0;
    int len = strlen(line);
    char buf[2];

    for (i = 0; i < len; i++){
        if(isdigit(line[i])){
            strncpy(buf, &line[i], 1*sizeof(char));
            printf("%s,",buf);
            return atoi(buf);
        }
    }
    return 0;
}

int last_digit(const char* line){
    int i = 0;
    int len = strlen(line);
    char buf[2];

    for (i = len - 1; 0 <= i; i--){
        if(isdigit(line[i])){
            strncpy(buf, &line[i], 1*sizeof(char));
            printf("%s\n",buf);
            return atoi(buf);
        }
    }
    return 0;
}

int calibration(const char* line) {
    return first_digit(line) * 10 + last_digit(line);
}


int main(int number, char** args)
{
    FILE *fp = fopen("input.txt", "r");
    char buf[100];
    int sum = 0;
    int cal = 0;
    while (!feof(fp)) {
        fgets(buf, sizeof(buf), fp);
        cal = calibration(buf);
        printf("%d\n",cal);
        sum += cal;
    };
    
    if(feof(fp))
        fclose(fp);

    printf("%d\n", sum);
    return 0;
}