import { GeneratorFactoryContext } from '@core/types'
import { DataModel } from '@zenstackhq/sdk/ast'
import { TypeScriptASTFactory } from '@utils/typescript/ast-factory'
import { TypeFormatter } from '@utils/schema/type-formatter'
import { UnifiedScalarGenerator } from '@generators/outputs/unified-scalar-generator'
import { UnifiedEnumGenerator } from '@generators/outputs/unified-enum-generator'
import { OutputFormat } from '@utils/constants'

export interface TypeScriptGenerationStats {
	objectTypes: string[]
	enumTypes: string[]
	scalarTypes: string[]
}

export interface TypeScriptGenerationResult {
	code: string
	stats: TypeScriptGenerationStats
}

export class CoreGenerator {
	private astFactory: TypeScriptASTFactory
	private context: GeneratorFactoryContext

	constructor(context: GeneratorFactoryContext) {
		this.context = context
		this.astFactory = new TypeScriptASTFactory(new TypeFormatter(context.options.typeNaming, context.options.fieldNaming))
	}

	generate(): TypeScriptGenerationResult {
		const { options, models } = this.context

		const objectTypes: string[] = []

		const scalarGenerator = new UnifiedScalarGenerator(this.context, OutputFormat.TYPE_GRAPHQL)
		const scalarResult = options.generateScalars ? scalarGenerator.generate() : { typescriptTypes: [] }

		const enumGenerator = new UnifiedEnumGenerator(this.context, OutputFormat.TYPE_GRAPHQL)
		const enumResult = options.generateEnums ? enumGenerator.generate() : { typescriptTypes: [] }

		const validModels = models.filter((model: DataModel) => !model.isAbstract)
		for (const model of validModels) {
			try {
				this.astFactory.createObjectType(model)
				objectTypes.push(model.name)
			} catch (error) {
				console.warn(`Failed to generate TypeScript object type for ${model.name}:`, error)
			}
		}

		const stats: TypeScriptGenerationStats = {
			objectTypes,
			enumTypes: enumResult.typescriptTypes,
			scalarTypes: scalarResult.typescriptTypes,
		}

		const code = this.astFactory.getGeneratedCode()

		return {
			code,
			stats,
		}
	}
}
