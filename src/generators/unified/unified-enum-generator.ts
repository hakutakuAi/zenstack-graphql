import { UnifiedGeneratorBase } from './unified-generator-base'
import { UnifiedGeneratorContext } from '@generators/strategies'
import { Enum } from '@zenstackhq/sdk/ast'
import { OutputFormat } from '@utils/constants'

export interface EnumGenerationResult {
	graphqlTypes: string[]
	typescriptTypes: string[]
}

export class UnifiedEnumGenerator extends UnifiedGeneratorBase {
	private format: OutputFormat

	constructor(context: UnifiedGeneratorContext, format: OutputFormat = OutputFormat.TYPE_GRAPHQL) {
		super(context)
		this.format = format
	}

	override generate(): string[] {
		const results: string[] = []

		for (const enumObj of this.enums) {
			try {
				const enumName = this.typeFormatter.formatTypeName(enumObj.name)
				this.outputStrategy.createEnumType(enumObj)
				results.push(enumName)
			} catch (error) {
				console.warn(`Failed to process enum ${enumObj.name}:`, error)
			}
		}

		return results
	}

	protected generateForModel(): string | null {
		return null
	}

	static getSupportedFormats(): OutputFormat[] {
		return [OutputFormat.GRAPHQL, OutputFormat.TYPE_GRAPHQL]
	}
}
