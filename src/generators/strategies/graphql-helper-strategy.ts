import { ModelHelper, HelperGenerationContext } from '@generators/unified/unified-helper-generator'
import { TypeScriptHelperStrategy } from './typescript-helper-strategy'

export class GraphQLHelperStrategy extends TypeScriptHelperStrategy {
	override generateHelpers(helpers: ModelHelper[], context: HelperGenerationContext): string[] {
		// Always generate TypeScript helpers, even for GraphQL output
		return super.generateHelpers(helpers, context)
	}
}