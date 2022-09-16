package org.dwcj.test;

import org.dwcj.Environment;
import org.junit.jupiter.api.Test;

public class SimpleTest {
    @Test
    public void test() {
        assert (Environment.isUnitTest());
    }

}