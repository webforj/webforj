package kotlin;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Test double for the marker the Kotlin compiler puts on every class it emits. Only the fully
 * qualified name matters to the detector, so the real Kotlin runtime is not needed on the test
 * classpath.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Metadata {
}
