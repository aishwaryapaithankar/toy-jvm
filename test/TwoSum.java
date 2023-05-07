public class TwoSum {
    public static int[] twoSum(float[] nums, float target) {
        for (int i = 0; i < nums.length; i++) {
            for (int j = i + 1; j < nums.length; j++) {
                if (nums[j] == target - nums[i]) {
                    return new int[] { i, j };
                }
            }
        }
        // In case there is no solution, we'll just return {-1,-1}
        return new int[] { -1, -1 };
    }
    public static void main(String[] args) {
        float[] nums = new float[100];
        for (int i = 0; i < nums.length; i++)
        	nums[i] = (float)(i+1);

        int[] ans = twoSum(nums,199f);
        System.out.println("Sum pair found at: " + ans);

        ans = twoSum(nums,4.5f);
        System.out.println("Sum pair found at: " + ans);
    }
}
