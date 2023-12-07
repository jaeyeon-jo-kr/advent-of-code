#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <regex.h>


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

int part1(){
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

    printf("day1 part 1 : %d\n", sum);
    return 0;
}

int is_matches(const char* src, const char* dest){
    return strncmp(src, dest, strlen(dest)) == 0;
}

int find_matches(const char* src){
    char numbers[9][100];
    strcpy(numbers[0], "one");
    strcpy(numbers[1], "two");
    strcpy(numbers[2], "three");
    strcpy(numbers[3], "four");
    strcpy(numbers[4], "five");
    strcpy(numbers[5], "six");
    strcpy(numbers[6], "seven");
    strcpy(numbers[7], "eight");
    strcpy(numbers[8], "nine");
    
    for(int i = 0;i<9;i++){
        if(is_matches(src, numbers[i])){
            return ++i;
        }
    }
    return 0;
}


int first_digit_part2(const char* line){
    int i = 0;
    int len = strlen(line);
    char buf[2];
    int matches = 0;

    for (i = 0; i < len; i++){
        if(isdigit(line[i])){
            strncpy(buf, &line[i], 1*sizeof(char));
            return atoi(buf);
        }else{
            matches = find_matches(&line[i]);
            if(matches)
                return matches;
        }
    }
    return 0;
}

int is_matches_rear(const char* src, const char* dest){
    int index = 0;
    if(strlen(src) < strlen(dest)){
        return 0;
    }

    index = strlen(src) - strlen(dest);
    // printf("match rear : %s, %s\n", &src[index], dest);

    return strncmp(&src[index], dest, strlen(dest)) == 0;
}
int find_matches_rear(const char* src){
    char numbers[9][100];
    strcpy(numbers[0], "one");
    strcpy(numbers[1], "two");
    strcpy(numbers[2], "three");
    strcpy(numbers[3], "four");
    strcpy(numbers[4], "five");
    strcpy(numbers[5], "six");
    strcpy(numbers[6], "seven");
    strcpy(numbers[7], "eight");
    strcpy(numbers[8], "nine");

    for(int i = 0;i<9;i++){
        if(is_matches_rear(src, numbers[i])){
            return ++i;
        }
    }
    return 0;
}


int last_digit_part2(const char* line){
    int i = 0;
    int len = strlen(line);
    char buf[2];
    char str_buf[100];
    int matches = 0;

    for (i = len - 1; 0 <= i; i--){
        if(isdigit(line[i])){
            strncpy(buf, &line[i], 1*sizeof(char));
            return atoi(buf);
        }else{
            memset(str_buf, 0, 100);
            strncpy(str_buf, line, i+1);
            matches = find_matches_rear(str_buf);
            if(matches)
                return matches;

        }
    }
    return 0;
}

int calibration_part2(const char* line) {
    return first_digit_part2(line) * 10 + last_digit_part2(line);
}

int part2(){
    FILE *fp = fopen("input.txt", "r");
    char buf[100];
    int sum = 0;
    int cal = 0;
    while (!feof(fp)) {
        fgets(buf, sizeof(buf), fp);
        cal = calibration_part2(buf);
        printf("%d\n",cal);
        sum += cal;
    };
    
    if(feof(fp))
        fclose(fp);

    printf("day1 part 2 : %d\n", sum);
    return 0;
    
}

int main(int number, char** args)
{
    // printf("%d\n", find_matches("one0"));
    // printf("%d\n", find_matches("two1"));
    // printf("%d\n", find_matches("three2"));
    // printf("%d\n\n", find_matches("four3"));
    
    // printf("%d\n", find_matches_rear("0one"));
    // printf("%d\n", find_matches_rear("0two"));
    // printf("%d\n", find_matches_rear("1three"));
    // printf("%d\n\n", find_matches_rear("1four"));

    // printf("%d\n", first_digit_part2("onethree"));
    // printf("%d\n", last_digit_part2("twofour"));
    // printf("%d\n", last_digit_part2("twothree"));
    // printf("%d\n", last_digit_part2("twothreeww"));
    part2();
   
    return 0;
}