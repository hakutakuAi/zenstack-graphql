import { ErrorCategory, ErrorHandler, ErrorSeverity } from './error-handler'

export { ErrorCategory as ErrorDecoratorCategory }

export interface ErrorOptions {
	category?: ErrorCategory
	suggestions?: string[]
	severity?: ErrorSeverity
}

export function Validate(options: ErrorOptions = {}) {
	return function (target: any, propertyKey: string, descriptor: PropertyDescriptor) {
		return wrapMethodWithErrorHandling(target, propertyKey, descriptor, {
			category: options.category || ErrorCategory.VALIDATION,
			suggestions: options.suggestions,
			severity: options.severity,
		})
	}
}

export function SchemaOp(options: ErrorOptions = {}) {
	return function (target: any, propertyKey: string, descriptor: PropertyDescriptor) {
		return wrapMethodWithErrorHandling(target, propertyKey, descriptor, {
			category: options.category || ErrorCategory.SCHEMA,
			suggestions: options.suggestions,
			severity: options.severity,
		})
	}
}

export function FileOp(options: ErrorOptions = {}) {
	return function (target: any, propertyKey: string, descriptor: PropertyDescriptor) {
		return wrapMethodWithErrorHandling(target, propertyKey, descriptor, {
			category: options.category || ErrorCategory.FILE,
			suggestions: options.suggestions,
			severity: options.severity,
		})
	}
}

export function Generate(options: ErrorOptions = {}) {
	return function (target: any, propertyKey: string, descriptor: PropertyDescriptor) {
		return wrapMethodWithErrorHandling(target, propertyKey, descriptor, {
			category: options.category || ErrorCategory.GENERATION,
			suggestions: options.suggestions,
			severity: options.severity,
		})
	}
}

export function HandleErrors(options: ErrorOptions = {}) {
	return function (target: any, propertyKey: string, descriptor: PropertyDescriptor) {
		return wrapMethodWithErrorHandling(target, propertyKey, descriptor, {
			category: options.category || ErrorCategory.PLUGIN,
			suggestions: options.suggestions,
			severity: options.severity,
		})
	}
}

function wrapMethodWithErrorHandling(target: any, propertyKey: string, descriptor: PropertyDescriptor, options: ErrorOptions) {
	const originalMethod = descriptor.value

	descriptor.value = function (...args: any[]) {
		try {
			return originalMethod.apply(this, args)
		} catch (error) {
			const instance = this as any
			const errorHandler = instance.errorHandler || new ErrorHandler()

			const operation = `${instance.constructor.name}.${propertyKey}`
			const errorMessage = `Error in ${operation}: ${error instanceof Error ? error.message : String(error)}`
			const context = { operation, originalError: error }

			const defaultSuggestions = [`Check input parameters for ${operation}`, 'Verify the operation preconditions are met']

			const suggestions = options.suggestions || defaultSuggestions
			const category = options.category || ErrorCategory.PLUGIN
			const severity = options.severity || ErrorSeverity.ERROR

			throw errorHandler.createError(errorMessage, category, severity, context, suggestions)
		}
	}

	return descriptor
}

export function SafeOperation<T>(errorHandler: ErrorHandler, operation: string, fn: () => T, options: ErrorOptions = {}): T {
	try {
		return fn()
	} catch (error) {
		const errorMessage = `Error in ${operation}: ${error instanceof Error ? error.message : String(error)}`
		const context = { operation, originalError: error }

		const defaultSuggestions = [`Check input parameters for ${operation}`, 'Verify the operation preconditions are met']

		const suggestions = options.suggestions || defaultSuggestions
		const category = options.category || ErrorCategory.GENERATION
		const severity = options.severity || ErrorSeverity.ERROR

		throw errorHandler.createError(errorMessage, category, severity, context, suggestions)
	}
}
