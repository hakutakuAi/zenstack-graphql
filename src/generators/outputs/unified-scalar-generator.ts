import { ScalarTypeComposer } from 'graphql-compose'
import { AbstractGenerator } from '@generators/abstract-generator'
import { TypeKind } from '@utils/registry/base-registry'
import {
	UNIFIED_SCALAR_DEFINITIONS,
	createGraphQLScalarType,
	createCustomScalarConfig,
	getScalarDefinition,
	isBuiltinGraphQLScalar,
	BUILTIN_PRISMA_TYPES,
	type ScalarConfig,
	type UnifiedScalarDefinition,
} from '@utils/constants/scalar-constants'
import { createGenerationContext, executeSafely } from '@utils/generator-utils'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'

import { OutputFormat } from '@utils/constants'

export interface ScalarGenerationResult {
	graphqlTypes: string[]
	typescriptTypes: string[]
}

export class UnifiedScalarGenerator extends AbstractGenerator<ScalarGenerationResult> {
	private astFactory?: TypeScriptASTFactory
	private format: OutputFormat

	constructor(context: any, format: OutputFormat = OutputFormat.GRAPHQL) {
		super(context)
		this.format = format

		if (format === OutputFormat.TYPE_GRAPHQL) {
			this.astFactory = new TypeScriptASTFactory(this.typeFormatter)
		}
	}

	generate(): ScalarGenerationResult {
		const result: ScalarGenerationResult = {
			graphqlTypes: [],
			typescriptTypes: [],
		}

		if (this.format === OutputFormat.GRAPHQL) {
			result.graphqlTypes = this.generateGraphQLScalars()
		} else {
			result.typescriptTypes = this.generateTypeScriptScalars()
		}

		return result
	}

	private generateGraphQLScalars(): string[] {
		this.registerBuiltInScalars()
		this.registerCustomScalars()
		return this.registry.getScalarTypes()
	}

	private generateTypeScriptScalars(): string[] {
		const results: string[] = []
		const scalarTypes = Object.values(this.options.scalarTypes)

		for (const scalarType of scalarTypes) {
			if (this.isCustomTypeScriptScalar(scalarType)) {
				const result = this.generateTypeScriptScalarType(scalarType)
				if (result) {
					results.push(result)
				}
			}
		}

		return results
	}

	private registerBuiltInScalars(): void {
		for (const definition of UNIFIED_SCALAR_DEFINITIONS) {
			const customName = this.options.scalarTypes[definition.prismaType]

			if (customName && customName !== definition.prismaType) {
				this.registerCustomNamedScalar(customName, definition)
			} else {
				this.registerBuiltInScalar(definition)
			}
		}
	}

	private registerBuiltInScalar(definition: UnifiedScalarDefinition): void {
		executeSafely(
			() => {
				const config = definition.createGraphQLConfig()

				if (this.hasScalarRegistered(config.name)) {
					return config.name
				}

				const scalarType = createGraphQLScalarType(config)
				const scalarComposer = ScalarTypeComposer.createTemp(scalarType)

				this.registerInSchemaComposer(config.name, scalarComposer)
				this.registry.registerType(config.name, TypeKind.SCALAR, scalarComposer, true)

				return config.name
			},
			createGenerationContext(definition.graphqlType, 'built-in scalar', 'register'),
		)
	}

	private registerCustomNamedScalar(customName: string, definition: UnifiedScalarDefinition): void {
		executeSafely(
			() => {
				const baseConfig = definition.createGraphQLConfig()
				const config: ScalarConfig = {
					...baseConfig,
					name: customName,
					description: `${baseConfig.description} (custom scalar type: ${customName})`,
				}

				if (this.hasScalarRegistered(config.name)) {
					return config.name
				}

				const scalarType = createGraphQLScalarType(config)
				const scalarComposer = ScalarTypeComposer.createTemp(scalarType)

				this.registerInSchemaComposer(config.name, scalarComposer)
				this.registry.registerType(config.name, TypeKind.SCALAR, scalarComposer, true)

				return config.name
			},
			createGenerationContext(customName, 'custom named scalar', 'register'),
		)
	}

	private registerCustomScalars(): void {
		Object.entries(this.options.scalarTypes).forEach(([prismaType, graphqlType]) => {
			if (this.shouldSkipCustomScalar(prismaType, graphqlType)) {
				return
			}

			executeSafely(
				() => {
					const config = createCustomScalarConfig(graphqlType, prismaType)

					if (this.hasScalarRegistered(config.name)) {
						return config.name
					}

					const scalarType = createGraphQLScalarType(config)
					const scalarComposer = ScalarTypeComposer.createTemp(scalarType)

					this.registerInSchemaComposer(config.name, scalarComposer)
					this.registry.registerType(config.name, TypeKind.SCALAR, scalarComposer, true)

					return config.name
				},
				createGenerationContext(graphqlType, 'custom scalar', 'register', { prismaType }),
			)
		})
	}

	private generateTypeScriptScalarType(scalarName: string): string | null {
		if (!this.astFactory) {
			return null
		}

		try {
			return this.astFactory.createScalarType(scalarName, scalarName)
		} catch (error) {
			return null
		}
	}

	private shouldSkipCustomScalar(prismaType: string, graphqlType: string): boolean {
		return BUILTIN_PRISMA_TYPES.includes(prismaType as any) || isBuiltinGraphQLScalar(graphqlType) || this.hasScalarRegistered(graphqlType)
	}

	private isCustomTypeScriptScalar(scalarType: string): boolean {
		return !isBuiltinGraphQLScalar(scalarType)
	}

	private hasScalarRegistered(name: string): boolean {
		return (this.schemaComposer && this.schemaComposer.has(name)) || this.registry.hasType(name) || this.registry.isTypeOfKind(name, TypeKind.SCALAR)
	}

	private registerInSchemaComposer(name: string, composer: ScalarTypeComposer): void {
		if (this.schemaComposer && !this.schemaComposer.has(name)) {
			this.schemaComposer.set(name, composer)
		}
	}

	static getAvailableScalarTypes(): string[] {
		return UNIFIED_SCALAR_DEFINITIONS.map((def) => def.prismaType)
	}

	static supportsScalarType(prismaType: string): boolean {
		return getScalarDefinition(prismaType) !== undefined
	}

	static getSupportedFormats(): OutputFormat[] {
		return [OutputFormat.GRAPHQL, OutputFormat.TYPE_GRAPHQL]
	}

	static createGraphQLGenerator(context: any): UnifiedScalarGenerator {
		return new UnifiedScalarGenerator(context, OutputFormat.GRAPHQL)
	}

	static createTypeScriptGenerator(context: any): UnifiedScalarGenerator {
		return new UnifiedScalarGenerator(context, OutputFormat.TYPE_GRAPHQL)
	}
}

export function createGraphQLScalarGenerator(context: any): UnifiedScalarGenerator {
	return UnifiedScalarGenerator.createGraphQLGenerator(context)
}

export function createTypeScriptScalarGenerator(context: any): UnifiedScalarGenerator {
	return UnifiedScalarGenerator.createTypeScriptGenerator(context)
}
