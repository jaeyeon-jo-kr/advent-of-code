from typing import List


class Solution:
    
    def explore(self, s:set[str], p:str):
        # print(p)
        # for l in s:
        #     print(l)
        if len(p) == 0:
            fin = {len(line) for line in s if len(line) == 0}
            return 0 < len(fin)
        elif len(p) == 1 or (1 < len(p) and p[1] != '*'):
            nexts = {line[1:]  for line in s if len(line) > 0 and self.canConsume(p[0], line[0])}
            return self.explore(nexts, p[1:])
        else:
            h = p[0]
            nexts = set()
            
            for line in s:
                nexts.add(line)
                for i, c in enumerate(line):
                    if self.canConsume(h, c):
                        nexts.add(line[i+1:])
                    else:
                        break
            return self.explore(nexts, p[2:])
        
        return False
                    
    
    def canConsume(self, ph:str, c:str):
        return ph == '.' or ph == c
        
        
    def isMatch(self, s: str, p: str) -> bool:
        return self.explore(set([s]), p)
    
sol = Solution()
print(0, sol.isMatch('a', 'a'))
print(1, sol.isMatch('a', 'a*'))
print(2, sol.isMatch('b', 'a'))
print(3, sol.isMatch('bcd', 'bc'))
print(4, sol.isMatch('bcd', 'bcd'))
print(5, sol.isMatch('bcaaad', 'bca*d'))
print(6, sol.isMatch('bcd', 'bca*d'))
print(7, sol.isMatch('bc', 'bc.*d'))
print(8, sol.isMatch('bce', 'bc.*e'))
# print(8, set([1]))
print('Hi')
