from typing import List


class Solution:
    def mergedSortedArray(self, nums1: List[int], nums2: List[int]) -> List[int]:
        merged = []
        merged_len = len(nums1) + len(nums2)
        while len(merged) < merged_len:
            if len(nums1) == 0:
                merged.append(nums2[0])
                nums2 = nums2[1:]
            elif len(nums2) == 0:
                merged.append(nums1[0])
                nums1 = nums1[1:]
            elif nums1[0] < nums2[0]:
                merged.append(nums1[0])
                nums1 = nums1[1:]
            else:
                merged.append(nums2[0])
                nums2 = nums2[1:]
        return merged
    
    def median(self, nums:List[int]):
        n = len(nums)
        if n % 2 == 0:
            return nums[int(n/2-1):int(n/2+1)]
        else:
            return nums[int(n/2):int(n/2+1)]
    
    def avg(self, nums:List[int]):
        return sum(nums)/len(nums)

    def findMedianSortedArrays(self, nums1: List[int], nums2: List[int]) -> float:
        sorted = self.mergedSortedArray(nums1, nums2)
        med = self.median(sorted)
        return self.avg(med)

sol = Solution()    
print(sol.findMedianSortedArrays([1,6], [3,7]))
sol.findMedianSortedArrays([1,2,3],[4,5,6])
        