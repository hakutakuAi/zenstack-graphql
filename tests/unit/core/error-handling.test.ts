import { describe, it, expect, beforeEach } from 'bun:test'
import {
	PluginError,
	ErrorCategory,
	warning,
	getErrorMessage,
	getErrorCategory,
	handleError,
	executeSafely,
	processArraySafely,
	createGenerationContext,
} from '@utils/error'

describe('Error Handling System', () => {
	describe('PluginError', () => {
		it('should create error with message and category', () => {
			const error = new PluginError('Test error', ErrorCategory.VALIDATION)

			expect(error.name).toBe('PluginError')
			expect(error.category).toBe(ErrorCategory.VALIDATION)
			expect(error.message).toContain('Test error')
		})

		it('should include context in error message', () => {
			const context = { modelName: 'User', fieldName: 'email' }
			const error = new PluginError('Test error', ErrorCategory.SCHEMA, context)

			expect(error.context).toEqual(context)
			expect(error.message).toContain('modelName: User')
			expect(error.message).toContain('fieldName: email')
		})

		it('should include suggestions in error message', () => {
			const suggestions = ['Check your schema', 'Verify field types']
			const error = new PluginError('Test error', ErrorCategory.VALIDATION, {}, suggestions)

			expect(error.suggestions).toEqual(suggestions)
			expect(error.message).toContain('Check your schema')
			expect(error.message).toContain('Verify field types')
		})

		it('should format complex context objects', () => {
			const context = {
				model: { name: 'User', fields: ['id', 'name'] },
				config: { generateScalars: true },
			}
			const error = new PluginError('Test error', ErrorCategory.GENERATION, context)

			expect(error.message).toContain('"name": "User"')
			expect(error.message).toContain('"generateScalars": true')
		})
	})

	describe('ErrorCategory', () => {
		it('should have correct error categories', () => {
			expect(ErrorCategory.VALIDATION).toBe(ErrorCategory.VALIDATION)
			expect(ErrorCategory.SCHEMA).toBe(ErrorCategory.SCHEMA)
			expect(ErrorCategory.FILE).toBe(ErrorCategory.FILE)
			expect(ErrorCategory.GENERATION).toBe(ErrorCategory.GENERATION)
			expect(ErrorCategory.PLUGIN).toBe(ErrorCategory.PLUGIN)
		})
	})

	describe('Warning Function', () => {
		it('should call console.warn with formatted message', () => {
			const originalWarn = console.warn
			const warnings: string[] = []
			console.warn = (message: string) => warnings.push(message)

			warning('Test warning', ErrorCategory.SCHEMA, { field: 'name' })

			expect(warnings).toHaveLength(1)
			expect(warnings[0]).toContain('[SCHEMA] Test warning')
			expect(warnings[0]).toContain('field: name')

			console.warn = originalWarn
		})
	})

	describe('Error Utilities', () => {
		it('getErrorMessage should extract message from Error', () => {
			const error = new Error('Test message')
			expect(getErrorMessage(error)).toBe('Test message')
		})

		it('getErrorMessage should convert non-Error to string', () => {
			expect(getErrorMessage('string error')).toBe('string error')
			expect(getErrorMessage(42)).toBe('42')
			expect(getErrorMessage(null)).toBe('null')
		})

		it('getErrorCategory should return category from PluginError', () => {
			const error = new PluginError('Test', ErrorCategory.VALIDATION)
			expect(getErrorCategory(error)).toBe(ErrorCategory.VALIDATION)
		})

		it('getErrorCategory should default to GENERATION for other errors', () => {
			const error = new Error('Test')
			expect(getErrorCategory(error)).toBe(ErrorCategory.GENERATION)
		})

		it('handleError should call warning with formatted message', () => {
			const originalWarn = console.warn
			const warnings: string[] = []
			console.warn = (message: string) => warnings.push(message)

			const error = new Error('Test error')
			handleError(error, 'process model', { modelName: 'User' })

			expect(warnings).toHaveLength(1)
			expect(warnings[0]).toContain('Failed to process model')
			expect(warnings[0]).toContain('Test error')
			expect(warnings[0]).toContain('modelName: User')

			console.warn = originalWarn
		})
	})

	describe('Safe Operation Execution', () => {
		it('executeSafely should return success for successful operation', () => {
			const operation = () => 'success result'
			const context = createGenerationContext('TestModel', 'object', 'generate')

			const result = executeSafely(operation, context)

			expect(result.success).toBe(true)
			expect(result.result).toBe('success result')
			expect(result.error).toBeUndefined()
		})

		it('executeSafely should return error for failed operation', () => {
			const originalWarn = console.warn
			console.warn = () => {}

			const operation = () => {
				throw new Error('Operation failed')
			}
			const context = createGenerationContext('TestModel', 'object', 'generate')

			const result = executeSafely(operation, context)

			expect(result.success).toBe(false)
			expect(result.result).toBeUndefined()
			expect(result.error).toBeInstanceOf(Error)
			expect(result.error!.message).toBe('Operation failed')

			console.warn = originalWarn
		})

		it('executeSafely should handle non-Error exceptions', () => {
			const originalWarn = console.warn
			console.warn = () => {}

			const operation = () => {
				throw 'String error'
			}
			const context = createGenerationContext('TestModel', 'object', 'generate')

			const result = executeSafely(operation, context)

			expect(result.success).toBe(false)
			expect(result.error).toBeInstanceOf(Error)
			expect(result.error!.message).toBe('String error')

			console.warn = originalWarn
		})
	})

	describe('Array Processing', () => {
		it('processArraySafely should process all successful items', () => {
			const items = [1, 2, 3, 4]
			const processor = (item: number) => ({
				success: true as const,
				result: item * 2,
			})

			const results = processArraySafely(items, processor)

			expect(results).toEqual([2, 4, 6, 8])
		})

		it('processArraySafely should skip failed items', () => {
			const items = [1, 2, 3, 4]
			const processor = (item: number) => ({
				success: item % 2 === 0,
				result: item % 2 === 0 ? item * 2 : undefined,
			})

			const results = processArraySafely(items, processor)

			expect(results).toEqual([4, 8])
		})

		it('processArraySafely should apply filter before processing', () => {
			const items = [1, 2, 3, 4, 5]
			const filter = (item: number) => item > 2
			const processor = (item: number) => ({
				success: true as const,
				result: item * 2,
			})

			const results = processArraySafely(items, processor, filter)

			expect(results).toEqual([6, 8, 10])
		})

		it('processArraySafely should handle empty arrays', () => {
			const results = processArraySafely([], () => ({ success: true, result: 'test' }))
			expect(results).toEqual([])
		})
	})

	describe('Generation Context', () => {
		it('should create context with required fields', () => {
			const context = createGenerationContext('User', 'model', 'generate')

			expect(context.itemName).toBe('User')
			expect(context.itemType).toBe('model')
			expect(context.operationType).toBe('generate')
			expect(context.additionalContext).toBeUndefined()
		})

		it('should include additional context when provided', () => {
			const additionalContext = { fieldCount: 5, hasRelations: true }
			const context = createGenerationContext('User', 'model', 'generate', additionalContext)

			expect(context.additionalContext).toEqual(additionalContext)
		})
	})

	describe('Error Message Formatting', () => {
		it('should format error with all components', () => {
			const error = new PluginError(
				'Generation failed',
				ErrorCategory.GENERATION,
				{
					modelName: 'User',
					fieldName: 'email',
					config: { generateScalars: true },
				},
				['Check your ZModel schema', 'Verify field types', 'Review plugin configuration'],
			)

			expect(error.message).toContain('[GENERATION] Generation failed')
			expect(error.message).toContain('Context:')
			expect(error.message).toContain('modelName: User')
			expect(error.message).toContain('fieldName: email')
			expect(error.message).toContain('generateScalars')
			expect(error.message).toContain('Suggestions:')
			expect(error.message).toContain('Check your ZModel schema')
			expect(error.message).toContain('Verify field types')
			expect(error.message).toContain('Review plugin configuration')
		})

		it('should handle empty context and suggestions', () => {
			const error = new PluginError('Simple error', ErrorCategory.VALIDATION)

			expect(error.message).toBe('[VALIDATION] Simple error')
		})
	})
})
