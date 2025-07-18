import { SchemaComposer } from 'graphql-compose'
import { printSchema, GraphQLSchema } from 'graphql'
import { ErrorHandler, GenerationError } from '@utils/error-handler'

export class SchemaBuilder {
	constructor(
		private readonly schemaComposer: SchemaComposer<any>,
		private readonly errorHandler: ErrorHandler = ErrorHandler.getInstance()
	) {}

	validateSchema(): string[] {
		try {
			this.schemaComposer.buildSchema()
			return []
		} catch (error) {
			return [`Schema validation failed: ${error instanceof Error ? error.message : String(error)}`]
		}
	}

	buildSchema(): GraphQLSchema {
		try {
			return this.schemaComposer.buildSchema({ keepUnusedTypes: true })
		} catch (error) {
			throw this.errorHandler.createGenerationError('Failed to build GraphQL schema', { originalError: error }, [
				'Check for invalid or missing type references',
				'Ensure all types have required fields',
				'Verify interface implementations are complete',
			])
		}
	}

	generateSDL(): string {
		try {
			const schema = this.buildSchema()
			return printSchema(schema)
		} catch (error) {
			if (error instanceof GenerationError) {
				throw error
			}

			throw this.errorHandler.createGenerationError('Failed to generate SDL', { originalError: error }, ['Ensure the schema can be built successfully first'])
		}
	}

	registerType(name: string, typeComposer: any): void {
		this.schemaComposer.set(name, typeComposer)
	}

	hasType(name: string): boolean {
		return this.schemaComposer.has(name)
	}

	getType(name: string): any | undefined {
		return this.schemaComposer.has(name) ? this.schemaComposer.get(name) : undefined
	}

	addRelayRequirements(): void {
		if (this.schemaComposer.has('Node')) {
			return
		}

		const nodeInterface = this.schemaComposer.createInterfaceTC({
			name: 'Node',
			description: 'An object with a unique identifier',
			fields: {
				id: {
					type: 'ID!',
					description: 'The unique identifier for this object',
				},
			},
		})

		this.schemaComposer.set('Node', nodeInterface)
	}
}
