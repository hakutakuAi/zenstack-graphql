import { expect } from 'bun:test'

export class TestUtilities {
	static assertGraphQLOutput(sdl: string | undefined, expectations: GraphQLExpectations): void {
		expect(sdl).toBeDefined()
		expect(typeof sdl).toBe('string')

		if (!sdl) return

		if (expectations.types) {
			expectations.types.forEach((typeName) => {
				expect(sdl).toContain(`type ${typeName}`)
			})
		}

		if (expectations.enums) {
			expectations.enums.forEach((enumName) => {
				expect(sdl).toContain(`enum ${enumName}`)
			})
		}

		if (expectations.scalars) {
			expectations.scalars.forEach((scalarName) => {
				expect(sdl).toContain(`scalar ${scalarName}`)
			})
		}

		if (expectations.inputs) {
			expectations.inputs.forEach((inputName) => {
				expect(sdl).toContain(`input ${inputName}`)
			})
		}

		if (expectations.fields) {
			Object.entries(expectations.fields).forEach(([typeName, fields]) => {
				fields.forEach((fieldName) => {
					const typeDefMatch = sdl.match(new RegExp(`type ${typeName}\\s*\\{[^}]+\\}`, 's'))
					expect(typeDefMatch).not.toBeNull()
					if (typeDefMatch) {
						expect(typeDefMatch[0]).toContain(fieldName)
					}
				})
			})
		}

		if (expectations.notContains) {
			expectations.notContains.forEach((content) => {
				expect(sdl).not.toContain(content)
			})
		}
	}

	static assertTypeScriptOutput(code: string | undefined, expectations: TypeScriptExpectations): void {
		expect(code).toBeDefined()
		expect(typeof code).toBe('string')

		if (expectations.classes) {
			expectations.classes.forEach((className) => {
				expect(code).toContain(`class ${className}`)
			})
		}

		if (expectations.decorators) {
			expectations.decorators.forEach((decorator) => {
				expect(code).toContain(`@${decorator}`)
			})
		}

		if (expectations.imports) {
			expectations.imports.forEach((importStatement) => {
				expect(code).toContain(importStatement)
			})
		}

		if (expectations.interfaces) {
			expectations.interfaces.forEach((interfaceName) => {
				expect(code).toContain(`interface ${interfaceName}`)
			})
		}

		if (expectations.notContains) {
			expectations.notContains.forEach((content) => {
				expect(code).not.toContain(content)
			})
		}
	}

	static assertGenerationStats(stats: any, expectations: StatsExpectations): void {
		expect(stats).toBeDefined()

		if (expectations.objectTypes !== undefined) {
			expect(stats.objectTypes).toBe(expectations.objectTypes)
		}

		if (expectations.enumTypes !== undefined) {
			expect(stats.enumTypes).toBe(expectations.enumTypes)
		}

		if (expectations.scalarTypes !== undefined) {
			expect(stats.scalarTypes).toBe(expectations.scalarTypes)
		}

		if (expectations.inputTypes !== undefined) {
			expect(stats.inputTypes).toBe(expectations.inputTypes)
		}

		if (expectations.connectionTypes !== undefined) {
			expect(stats.connectionTypes).toBe(expectations.connectionTypes)
		}

		if (expectations.totalTypes !== undefined) {
			expect(stats.totalTypes).toBe(expectations.totalTypes)
		}

		if (expectations.generationTimeMs !== undefined) {
			expect(stats.generationTimeMs).toBeGreaterThan(0)
			expect(stats.generationTimeMs).toBeLessThan(expectations.generationTimeMs)
		}

		if (expectations.minObjectTypes !== undefined) {
			expect(stats.objectTypes).toBeGreaterThanOrEqual(expectations.minObjectTypes)
		}

		if (expectations.minInputTypes !== undefined) {
			expect(stats.inputTypes).toBeGreaterThanOrEqual(expectations.minInputTypes)
		}
	}

	static assertArrayContains<T>(array: T[], expectedItems: T[]): void {
		expectedItems.forEach((item) => {
			expect(array).toContain(item)
		})
	}

	static assertArrayExactly<T>(array: T[], expectedItems: T[]): void {
		expect(array.length).toBe(expectedItems.length)
		expectedItems.forEach((item) => {
			expect(array).toContain(item)
		})
	}

	static assertFieldMapping(generatorResult: any, modelName: string, expectedFields: FieldExpectation[]): void {
		const modelCall = generatorResult.find((call: any) => call.args[0] === modelName)
		expect(modelCall).toBeDefined()

		const fields = modelCall.args[1]
		expectedFields.forEach((expectation) => {
			const field = fields.find((f: any) => f.name === expectation.name)
			expect(field).toBeDefined()

			if (expectation.type) {
				expect(field.type).toBe(expectation.type)
			}

			if (expectation.nullable !== undefined) {
				expect(field.nullable).toBe(expectation.nullable)
			}

			if (expectation.description) {
				expect(field.description).toContain(expectation.description)
			}
		})
	}

	static assertGeneratorResult(result: string[], expectedTypes: string[]): void {
		expect(result).toBeDefined()
		expect(Array.isArray(result)).toBe(true)

		if (expectedTypes.length > 0) {
			TestUtilities.assertArrayContains(result, expectedTypes)
		}

		result.forEach((typeName) => {
			expect(typeof typeName).toBe('string')
			expect(typeName.length).toBeGreaterThan(0)
		})
	}

	static createPerformanceThreshold(maxTimeMs: number): PerformanceThreshold {
		return {
			maxTimeMs,
			assertWithinThreshold: (actualTimeMs: number) => {
				expect(actualTimeMs).toBeLessThan(maxTimeMs)
				expect(actualTimeMs).toBeGreaterThan(0)
			},
		}
	}
}

export interface GraphQLExpectations {
	types?: string[]
	enums?: string[]
	scalars?: string[]
	inputs?: string[]
	fields?: Record<string, string[]>
	notContains?: string[]
}

export interface TypeScriptExpectations {
	classes?: string[]
	decorators?: string[]
	imports?: string[]
	interfaces?: string[]
	notContains?: string[]
}

export interface StatsExpectations {
	objectTypes?: number
	enumTypes?: number
	scalarTypes?: number
	inputTypes?: number
	connectionTypes?: number
	totalTypes?: number
	generationTimeMs?: number
	minObjectTypes?: number
	minInputTypes?: number
}

export interface FieldExpectation {
	name: string
	type?: string
	nullable?: boolean
	description?: string
}

export interface PerformanceThreshold {
	maxTimeMs: number
	assertWithinThreshold: (actualTimeMs: number) => void
}
