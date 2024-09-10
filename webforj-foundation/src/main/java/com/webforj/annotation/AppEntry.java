package com.webforj.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * {@code @AppEntry} is used to mark a class as the primary entry point of the application in the
 * webforJ framework.
 *
 * <p>
 * This annotation should be applied to classes that extend {@code com.webforj.App}. It indicates
 * that the annotated class is intended to serve as the main entry point when multiple subclasses of
 * {@code App} are present. If no class is annotated with {@code @AppEntry}, the framework will
 * default to the first discovered subclass of {@code App}.
 * </p>
 *
 * <p>
 * <strong>Usage Example:</strong>
 * </p>
 *
 * <pre>
 * {@literal @AppEntry}
 * public class MainApp extends App {
 *
 * }
 * </pre>
 *
 * @see com.webforj.App
 *
 * @author Hyyan Abo Fakher
 * @since 24.12
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Documented
public @interface AppEntry {
}
